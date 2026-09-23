require lib/test.f
require lib/engine-candidate.f
require lib/process-env.f
require test/snapshot-file.f

\ A dense input must keep dense geometry even on a v10 writer. This exercises
\ its size decision with 1 MiB of bytes, without adding that storage to DATA.
package SNAP
public
: TEST-DENSE-CHOICE ( -- )
   1048576 SDL ! 0 STSZ ! 0 SCL ! SND-ALLOC
   SDL @ CELL / 0 ?do -1 SND-PTR i cells + CELL-VIEW ! loop
   ENCODE-DATA
   SVER @ ADDRESS-CELLS:SNAPSHOT-VERSION T=
   SDW @ SDL @ T=
   SND-PTR SDL @ PROT-PAGE-MAX + munmap 0 T=
   0 SND-N ! ;
;package

package SNAPSHOT-FILE
$8000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U
create PATH-BUF FS-PATH-CAP allot
DYNAMIC-BUFFER FIRST-IMAGE n
variable FIRST-SIZE
create SAVED-VALUE IMAGE-CELLS:VMAX allot

: PATH ( ptr u8 n -- ptr u8 n ) {: name:ptr size:n :}
   ROOT-BUF ROOT-U @ name size PATH-BUF JOIN-PATH PATH-BUF swap ;
: APP$ ( -- ptr u8 n ) s" compressed" PATH ;
: AGAIN$ ( -- ptr u8 n ) s" recaptured" PATH ;
: DENSE$ ( -- ptr u8 n ) s" dense" PATH ;
: BAD$ ( -- ptr u8 n ) s" corrupt" PATH ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: out:len err:len :}
         out LEN>N OUT-U ! err LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: out:len err:len rc:rc :}
         out LEN>N OUT-U ! err LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: EXEC ( ptr u8 n ptr u8 n -- ) {: exe:ptr exeu:n src:ptr srcu:n :}
   exe exeu >LEN src srcu >LEN
   OUT CAP >LEN ERR CAP >LEN 180000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT ;

: CLEAN ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ERR-U @ 0 T= ;

: ARGS ( -- ) PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING ;
: DEST ( ptr u8 n -- )
   ARGS s" --" >LEN PROC-ARGV+ >LEN PROC-ARGV+ ;

: BUILD ( -- )
   APP$ DEST
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire test/snapshot-compression-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   EXEC CLEAN OUT-U @ 0 T= ;

: RUN-IMAGE ( ptr u8 n -- )
   ARGS S\" SNAPSHOT-COMPRESSION-SUBJECT:VERIFY\n" EXEC CLEAN
   OUT OUT-U @ S\" snapshot compression: ok\n" T$= ;

: BAD-IMAGE ( -- )
   BAD$ IMAGE@ IMAGE-U @ WRITE-ALL
   BAD$ CODESIGN:FORCE
   ARGS BAD$ NULL$ EXEC
   RC @ 79 T=
   ERR ERR-U @ s" snapshot trailer corrupt" CONTAINS? TTRUE ;

: BAD-BYTE ( n n -- ) {: off:n val:n :}
   IMAGE@ off + c@ {: old:n :}
   val IMAGE@ off + c! BAD-IMAGE old IMAGE@ off + c! ;

: MALFORMED ( -- )
   s" the decoded extent is bounded before allocation" T-LABEL
   DATA-OFF @ 7 + $7F BAD-BYTE
   s" a presence-map group beyond DATA is refused" T-LABEL
   DATA-OFF @ 8 + 3 + $7F BAD-BYTE
   s" the stored bitmap consists of complete groups" T-LABEL
   DATA-OFF @ 12 + 1 BAD-BYTE
   s" a present value cannot be zero" T-LABEL
   IMAGE@ DATA-OFF @ + 8 + IMAGE-CELLS:U32@ IMAGE-CELLS:PMAP-BYTES
   IMAGE@ DATA-OFF @ + 12 + IMAGE-CELLS:U32@ + DATA-OFF @ 16 + + {: val:n :}
   val 0 BAD-BYTE
   s" a value cannot continue beyond ten bytes" T-LABEL
   IMAGE@ val + SAVED-VALUE IMAGE-CELLS:VMAX BYTE-COPY
   IMAGE-CELLS:VMAX 0 ?do $80 IMAGE@ val + i + c! loop
   BAD-IMAGE
   SAVED-VALUE IMAGE@ val + IMAGE-CELLS:VMAX BYTE-COPY
   s" padding inside the stored extent must be zero" T-LABEL
   TRAILER @ DATA-OFF @ PAYLOAD-U @ + > TTRUE
   TRAILER @ 1- 1 BAD-BYTE ;

: BODY ( -- )
   SNAP-FORMAT-VERSION ADDRESS-CELLS:SNAPSHOT-VERSION > TTRUE
   s" writer keeps the smaller dense representation" T-LABEL
   SNAP:TEST-DENSE-CHOICE
   s" snapshot-compression" HB-TMP-MKDIR {: root:ptr rootu:n :}
   root ROOT-BUF rootu BYTE-COPY rootu ROOT-U ! root rootu CLEANUP-TREE+
   BUILD APP$ RUN-IMAGE APP$ READ-IMAGE
   s" sparse snapshots use v10 and restore every byte of the hole" T-LABEL
   FORMAT SNAP-FORMAT-VERSION T=
   DATA-U @ TRAILER @ DATA-OFF @ - > TTRUE
   IMAGE-U @ dup FIRST-SIZE ! CELLS-FOR FIRST-IMAGE-RESERVE
   IMAGE@ 0 FIRST-IMAGE BYTE-VIEW IMAGE-U @ BYTE-COPY
   BUILD APP$ READ-IMAGE
   s" canonical encoding gives identical images from identical captures" T-LABEL
   IMAGE-U @ FIRST-SIZE @ T=
   IMAGE@ IMAGE-U @ 0 FIRST-IMAGE BYTE-VIEW FIRST-SIZE @ STR= TTRUE
   FIRST-IMAGE-RELEASE
   MALFORMED
   s" dense v9 remains readable with the unchanged address-cell schema" T-LABEL
   ADDRESS-CELLS:SNAPSHOT-VERSION VERSION ! DENSE$ WRITE
   DENSE$ RUN-IMAGE
   s" recapture boots without nesting the previous image" T-LABEL
   AGAIN$ DEST APP$ S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" EXEC CLEAN
   AGAIN$ RUN-IMAGE AGAIN$ FILE-SIZE FIRST-SIZE @ PROT-PAGE-MAX + <= TTRUE ;

: CLEANUP ( -- ) RELEASE FIRST-IMAGE-RELEASE CLEANUP-RUN ;
: RUN ( -- )
   T-RESET CLEANUP-RESET [: BODY ;] [: CLEANUP ;] finally T-REPORT ;
RUN
;package
