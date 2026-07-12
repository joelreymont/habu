\ run-result-cache.f - content-keyed PASS-stamp store for gate phases.
\
\ Pure stamp mechanism: the suite builds a phase key with lib/content-key.f,
\ asks TRC:HIT? before starting the phase, records misses with TRC:PENDING+,
\ and writes stamps with TRC:STAMP+ only after a fully green run. Suite glue
\ (key contents, bypass flags, red policy) lives in test/run-lib.f.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f

package TRC

64 constant KEY-HEX-U
$40 constant PENDING-CAP
$50 constant NAME-CAP

create ROOT-BUF FS-PATH-CAP allot
create NAME-BUF NAME-CAP allot
create PATH-BUF FS-PATH-CAP allot
create TMP-BUF FS-PATH-CAP allot
create PENDING-KEYS PENDING-CAP KEY-HEX-U * allot
create PENDING-PHASES PENDING-CAP cells allot

variable ROOT-LEN
variable NAME-LEN
variable PATH-LEN
variable TMP-LEN
variable PENDING-LEN

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-LEN @ ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-LEN @ ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-LEN @ ;

: TMP$ ( -- ptr u8 n )
   TMP-BUF TMP-LEN @ ;

: NAME! ( ptr u8 -- ) {: hex:ptr :}
   s" gate-pass-" {: p:ptr pu:n :}
   pu KEY-HEX-U + NAME-CAP > if E-STR-BOUNDS throw then
   p NAME-BUF pu BYTE-COPY
   hex NAME-BUF pu + KEY-HEX-U BYTE-COPY
   pu KEY-HEX-U + NAME-LEN ! ;

: TMP-NAME! ( -- )
   s" .tmp" {: s:ptr su:n :}
   NAME-LEN @ su + NAME-CAP > if E-STR-BOUNDS throw then
   s NAME-BUF NAME-LEN @ + su BYTE-COPY
   NAME-LEN @ su + NAME-LEN ! ;

: PATHS! ( ptr u8 -- ) {: hex:ptr :}
   hex NAME!
   ROOT$ NAME$ PATH-BUF JOIN-PATH PATH-LEN !
   TMP-NAME!
   ROOT$ NAME$ TMP-BUF JOIN-PATH TMP-LEN ! ;

public

: RESET ( -- )
   0 PENDING-LEN ! ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-LEN ! ;

: ROOT? ( -- bool )
   ROOT-LEN @ 0 > ;

: HIT? ( ptr u8 -- bool )
   PATHS!
   PATH$ FILE? ;

: STAMP+ ( ptr u8 n ptr u8 -- ) {: label:ptr labelu:n hex:ptr :}
   hex PATHS!
   ROOT$ MAKE-DIRS
   TMP$ label labelu WRITE-ALL
   TMP$ PATH$ RENAME-FILE ;

: PENDING+ ( n ptr u8 -- ) {: phase:n hex:ptr :}
   PENDING-LEN @ PENDING-CAP >= if E-TBL-BOUNDS throw then
   hex PENDING-KEYS PENDING-LEN @ KEY-HEX-U * + KEY-HEX-U BYTE-COPY
   phase PENDING-LEN @ cells PENDING-PHASES + !
   PENDING-LEN @ 1 + PENDING-LEN ! ;

: PENDING# ( -- n )
   PENDING-LEN @ ;

: PENDING-PHASE ( n -- n ) {: i:n :}
   i 0 < if E-TBL-BOUNDS throw then
   i PENDING-LEN @ >= if E-TBL-BOUNDS throw then
   i cells PENDING-PHASES + @ ;

: PENDING-KEY ( n -- ptr u8 ) {: i:n :}
   i 0 < if E-TBL-BOUNDS throw then
   i PENDING-LEN @ >= if E-TBL-BOUNDS throw then
   PENDING-KEYS i KEY-HEX-U * + ;

;package
