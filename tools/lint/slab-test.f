\ Replacement mappings must be released, while smaller loads reuse storage.
require lib/test.f
require lib/test/mapped.f
require tools/lint/text.f

package LINT-SLAB-TEST

create SLAB LINT-SLAB:CELLS cells allot
create DIR FS-PATH-CAP allot
variable DIR-U
create PATH FS-PATH-CAP allot
variable PATH-U
140000 constant SIZE
create SOURCE SIZE allot

: PATH$ ( -- ptr u8 n ) PATH PATH-U @ ;
: LOAD-SIZE ( n -- ) {: size :}
   PATH$ SOURCE size WRITE-ALL
   PATH$ SLAB LINT-SLAB:LOAD
   SLAB LINT-SLAB:TEXT SOURCE size T$= ;

: GROW ( -- )
   1 LOAD-SIZE SLAB LINT-SLAB:TEXT drop {: first:ptr :}
   first MAPPED:LIVE? TTRUE
   70000 LOAD-SIZE SLAB LINT-SLAB:TEXT drop {: second:ptr :}
   first MAPPED:LIVE? TFALSE second MAPPED:LIVE? TTRUE
   SIZE LOAD-SIZE SLAB LINT-SLAB:TEXT drop {: third:ptr :}
   second MAPPED:LIVE? TFALSE third MAPPED:LIVE? TTRUE
   17 LOAD-SIZE SLAB LINT-SLAB:TEXT drop third = TTRUE
   0 LOAD-SIZE SLAB LINT-SLAB:TEXT drop third = TTRUE
   third MAPPED:LIVE? TTRUE ;

: CLEAN ( -- )
   PATH$ EXISTS? if PATH$ REMOVE-FILE then
   DIR DIR-U @ REMOVE-DIR ;

: RUN ( -- )
   T-RESET
   SIZE 0 ?do i 255 and SOURCE i + c! loop
   s" habu-slab" TMPDIR-MKDIR {: a:ptr u :}
   a DIR u BYTE-COPY u DIR-U !
   DIR DIR-U @ s" source" PATH JOIN-PATH PATH-U !
   [: GROW ;] [: CLEAN ;] finally
   T-REPORT ;

RUN
;package
