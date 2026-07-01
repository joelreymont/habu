\ rig-geometry.f - Habu rig extrinsics survey/readiness tools.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/float.f
require lib/memory.f
require lib/prelude.f
require tools/json.f
require odin/float-cell.f

package RIGGEO
private

8 constant RG-MAX-ROWS
4 constant RG-MAX-CAMERAS
1024 constant RG-PATH-CAP
256 constant RG-TEXT-CAP
$40000 constant RG-READ-CAP
11 constant RG-SURVEY-FIELDS

-8700 constant E-RG-JSON
-8701 constant E-RG-CAPACITY
-8702 constant E-RG-SURVEY
-8703 constant E-RG-USAGE

create RG-EXTRINSICS-BUF RG-PATH-CAP allot
create RG-SURVEY-BUF RG-PATH-CAP allot
create RG-OUTPUT-BUF RG-PATH-CAP allot
create RG-P0 RG-PATH-CAP allot
create RG-P1 RG-PATH-CAP allot
create RG-READ-BUF RG-READ-CAP allot
create RG-SCOPE-BUF RG-MAX-ROWS RG-TEXT-CAP * allot
create RG-NAME-BUF RG-MAX-ROWS RG-TEXT-CAP * allot
create RG-STATUS-BUF RG-MAX-ROWS RG-TEXT-CAP * allot
create RG-REASON-BUF RG-MAX-ROWS RG-TEXT-CAP * allot
create RG-SCOPE-U RG-MAX-ROWS cells allot
create RG-NAME-U RG-MAX-ROWS cells allot
create RG-STATUS-U RG-MAX-ROWS cells allot
create RG-REASON-U RG-MAX-ROWS cells allot
create RG-TX RG-MAX-ROWS cells allot
create RG-TY RG-MAX-ROWS cells allot
create RG-TZ RG-MAX-ROWS cells allot
create RG-RX RG-MAX-ROWS cells allot
create RG-RY RG-MAX-ROWS cells allot
create RG-RZ RG-MAX-ROWS cells allot
create RG-RES RG-MAX-ROWS cells allot
create RG-HAVE-T RG-MAX-ROWS cells allot
create RG-HAVE-R RG-MAX-ROWS cells allot
create RG-HAVE-RES RG-MAX-ROWS cells allot
create RG-RES-REQ RG-MAX-ROWS cells allot
create RG-READY RG-MAX-ROWS cells allot
create RG-FIELD-A RG-SURVEY-FIELDS cells allot
create RG-FIELD-U RG-SURVEY-FIELDS cells allot

variable RG-EXTRINSICS-U
variable RG-SURVEY-U
variable RG-OUTPUT-U
variable RG-READ-U
variable RG-ROWS
variable RG-CAMERAS
variable RG-READY-CAMS
variable RG-READY-XFORMS
variable RG-DUP-NAMES
variable RG-SCHEMA-READY
variable RG-CAMERA-COUNT-READY
variable RG-REQUIRE-READY
variable RG-I
variable RG-J
variable RG-K
variable RG-LINE-A
variable RG-LINE-U
variable RG-SRC-A
variable RG-SRC-U
variable RG-SAW-HEADER
variable RG-SAW-RIG
variable RG-SURVEY-CAMS

: RG-TRUE ( -- bool ) 0 0= ;
: RG-FALSE ( -- bool ) 0 0= 0= ;

: A@ ( ptr a n -- n ) {: base:ptr ix:n :} base ix cells + @ ;
: A! ( n ptr a n -- ) {: v:n base:ptr ix:n :} v base ix cells + ! ;
: FA@ ( ptr a n -- r ) {: base:ptr ix:n :} base ix cells + F@ ;
: FA! ( r ptr a n -- ) {: v:r base:ptr ix:n :} v base ix cells + F! ;
: PTRA-SLOT ( ptr a n -- ptr ptr u8 ) {: base:ptr ix:n :} base ix ptr-field ;
: PTRA@ ( ptr a n -- ptr u8 ) PTRA-SLOT @ ;
: PTRA! ( ptr u8 ptr a n -- ) {: a:ptr base:ptr ix:n :} a base ix PTRA-SLOT ! ;
: SRC-A-FIELD ( -- ptr ptr u8 ) RG-SRC-A 0 ptr-field ;
: SRC-A@ ( -- ptr u8 ) SRC-A-FIELD @ ;
: SRC-A! ( ptr u8 -- ) SRC-A-FIELD ! ;

: RG-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-RG-CAPACITY throw then
   u cap >= if E-RG-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: SLOT ( n ptr u8 -- ptr u8 )
   swap RG-TEXT-CAP * + ;

: EXTRINSICS$ ( -- ptr u8 n ) RG-EXTRINSICS-BUF RG-EXTRINSICS-U @ ;
: SURVEY$ ( -- ptr u8 n ) RG-SURVEY-BUF RG-SURVEY-U @ ;
: OUTPUT$ ( -- ptr u8 n ) RG-OUTPUT-BUF RG-OUTPUT-U @ ;
: SCOPE$ ( n -- ptr u8 n )
   {: ix:n :}
   ix RG-SCOPE-BUF SLOT
   RG-SCOPE-U ix A@ ;

: NAME$ ( n -- ptr u8 n )
   {: ix:n :}
   ix RG-NAME-BUF SLOT
   RG-NAME-U ix A@ ;

: STATUS$ ( n -- ptr u8 n )
   {: ix:n :}
   ix RG-STATUS-BUF SLOT
   RG-STATUS-U ix A@ ;

: REASON$ ( n -- ptr u8 n )
   {: ix:n :}
   ix RG-REASON-BUF SLOT
   RG-REASON-U ix A@ ;

: SET-EXTRINSICS ( ptr u8 n -- ) RG-EXTRINSICS-BUF RG-PATH-CAP RG-EXTRINSICS-U RG-COPY! ;
: SET-SURVEY ( ptr u8 n -- ) RG-SURVEY-BUF RG-PATH-CAP RG-SURVEY-U RG-COPY! ;
: SET-OUTPUT ( ptr u8 n -- ) RG-OUTPUT-BUF RG-PATH-CAP RG-OUTPUT-U RG-COPY! ;

: ROW-COPY ( ptr u8 n ptr u8 ptr a -- )
   RG-TEXT-CAP swap RG-COPY! ;

: ROW-SCOPE! ( ptr u8 n n -- )
   {: ix:n :} ix RG-SCOPE-BUF SLOT RG-SCOPE-U ix cells + ROW-COPY ;

: ROW-NAME! ( ptr u8 n n -- )
   {: ix:n :} ix RG-NAME-BUF SLOT RG-NAME-U ix cells + ROW-COPY ;

: ROW-STATUS! ( ptr u8 n n -- )
   {: ix:n :} ix RG-STATUS-BUF SLOT RG-STATUS-U ix cells + ROW-COPY ;

: ROW-REASON! ( ptr u8 n n -- )
   {: ix:n :} ix RG-REASON-BUF SLOT RG-REASON-U ix cells + ROW-COPY ;

: PATH-IN-OUT$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   OUTPUT$ name nameu RG-P0 JOIN-PATH RG-P0 swap ;

: RG-RESET-RESULTS ( -- )
   0 RG-READ-U !
   0 RG-ROWS !
   0 RG-CAMERAS !
   0 RG-READY-CAMS !
   0 RG-READY-XFORMS !
   0 RG-DUP-NAMES !
   0 RG-SCHEMA-READY !
   0 RG-CAMERA-COUNT-READY !
   0 RG-REQUIRE-READY ! ;

: RG-RESET ( -- )
   s" results/rig_geometry/extrinsics_initial.json" SET-EXTRINSICS
   s" configs/rig_survey.example.csv" SET-SURVEY
   s" results/rig_geometry/readiness" SET-OUTPUT
   RG-RESET-RESULTS ;

: JGET ( n ptr u8 n -- n )
   JSON-GET dup -1 = if E-RG-JSON throw then ;

: JSTR$ ( n ptr u8 n -- ptr u8 n )
   JGET dup JSON-KIND J-STR <> if E-RG-JSON throw then JSON-STRING$ ;

: JNUM>R ( n -- r )
   dup JSON-KIND J-NUM <> if E-RG-JSON throw then
   JSON-NUMBER$ STR>FLOAT 0= if drop E-RG-JSON throw then ;

: JARR3 ( n ptr u8 n ptr a ptr a ptr a ptr n -- )
   {: root:n key:ptr keyu:n x:ptr y:ptr z:ptr present:ptr :}
   0 present !
   root key keyu JGET {: arr:n :}
   arr JSON-KIND J-NULL = if exit then
   arr JSON-KIND J-ARR <> if E-RG-JSON throw then
   arr JSON-COUNT 3 <> if E-RG-JSON throw then
   arr 0 JSON-ARR@ JNUM>R x F!
   arr 1 JSON-ARR@ JNUM>R y F!
   arr 2 JSON-ARR@ JNUM>R z F!
   1 present ! ;

: JOPT-R ( n ptr u8 n ptr a ptr n -- )
   {: root:n key:ptr keyu:n dst:ptr present:ptr :}
   0 present !
   root key keyu JGET {: v:n :}
   v JSON-KIND J-NULL = if exit then
   v JNUM>R dst F!
   1 present ! ;

: STATUS-OK? ( ptr u8 n -- bool )
   2dup s" measured" STR= if 2drop RG-TRUE exit then
   s" estimated" STR= ;

: UNKNOWN-STATUS? ( ptr u8 n -- bool )
   s" unknown" STARTS-WITH? ;

: DUP-NAME? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   1 RG-J !
   begin RG-J @ RG-ROWS @ < while
      a u RG-J @ NAME$ STR= if RG-TRUE exit then
      RG-J @ 1+ RG-J !
   repeat
   RG-FALSE ;

: ROW-READY! ( bool n -- )
   {: ready:bool ix:n :}
   ready if 1 else 0 then RG-READY ix A! ;

: ROW-CLASSIFY ( n bool bool -- )
   {: ix:n resreq:bool duplicate:bool :}
   RG-TRUE ix ROW-READY!
   s" ready" ix ROW-REASON!
   ix NAME$ dup 0= if
      2drop RG-FALSE ix ROW-READY! s" missing logical name" ix ROW-REASON! exit
   then
   2drop
   duplicate if
      RG-FALSE ix ROW-READY! s" duplicate logical name" ix ROW-REASON! exit
   then
   ix STATUS$ 2dup UNKNOWN-STATUS? if
      2drop RG-FALSE ix ROW-READY! s" unknown transform status" ix ROW-REASON! exit
   then
   STATUS-OK? 0= if
      RG-FALSE ix ROW-READY! s" unsupported transform status" ix ROW-REASON! exit
   then
   RG-HAVE-T ix A@ 0= if
      RG-FALSE ix ROW-READY! s" missing translation" ix ROW-REASON! exit
   then
   RG-HAVE-R ix A@ 0= if
      RG-FALSE ix ROW-READY! s" missing rotation" ix ROW-REASON! exit
   then
   resreq if
      RG-HAVE-RES ix A@ 0= if
         RG-FALSE ix ROW-READY! s" missing residual" ix ROW-REASON! exit
      then
   then ;

: ROW-ADD ( ptr u8 n ptr u8 n n bool -- )
   {: scope:ptr scopeu:n name:ptr nameu:n xform:n resreq:bool :}
   RG-ROWS @ RG-MAX-ROWS >= if E-RG-CAPACITY throw then
   RG-ROWS @ {: ix:n :}
   scope scopeu ix ROW-SCOPE!
   name nameu ix ROW-NAME!
   xform s" status" JSTR$ ix ROW-STATUS!
   xform s" translation_m" RG-TX ix cells + RG-TY ix cells + RG-TZ ix cells + RG-HAVE-T ix cells + JARR3
   xform s" rotation_rpy_deg" RG-RX ix cells + RG-RY ix cells + RG-RZ ix cells + RG-HAVE-R ix cells + JARR3
   xform s" residual" RG-RES ix cells + RG-HAVE-RES ix cells + JOPT-R
   resreq if 1 else 0 then RG-RES-REQ ix A!
   name nameu DUP-NAME? {: dupname:bool :}
   dupname if RG-DUP-NAMES @ 1+ RG-DUP-NAMES ! then
   ix resreq dupname ROW-CLASSIFY
   RG-READY ix A@ 0 <> if
      RG-READY-XFORMS @ 1+ RG-READY-XFORMS !
      resreq if RG-READY-CAMS @ 1+ RG-READY-CAMS ! then
   then
   resreq if RG-CAMERAS @ 1+ RG-CAMERAS ! then
   RG-ROWS @ 1+ RG-ROWS ! ;

: PARSE-EXTRINSICS ( ptr u8 n -- )
   RG-RESET-RESULTS
   JSON-PARSE {: root:n :}
   root s" schema_version" JSTR$ s" odin.rig_geometry.extrinsics_initial.v1" STR= if 1 RG-SCHEMA-READY ! then
   s" rig" s" rig_to_truck" root s" rig_to_truck" JGET RG-FALSE ROW-ADD
   root s" cameras" JGET {: cams:n :}
   cams JSON-KIND J-ARR <> if E-RG-JSON throw then
   0 RG-I !
   begin RG-I @ cams JSON-COUNT < while
      cams RG-I @ JSON-ARR@ {: cam:n :}
      s" camera"
      cam s" logical_name" JSTR$
      cam s" camera_to_rig" JGET
      RG-TRUE ROW-ADD
      RG-I @ 1+ RG-I !
   repeat
   RG-CAMERAS @ RG-MAX-CAMERAS = if 1 RG-CAMERA-COUNT-READY ! then ;

: RESULT$ ( -- ptr u8 n )
   RG-SCHEMA-READY @ 0 <> RG-CAMERA-COUNT-READY @ 0 <> and
   RG-DUP-NAMES @ 0= and
   RG-READY-XFORMS @ RG-ROWS @ = and
   if s" pass" else s" fail" then ;

: YESNO ( bool -- ptr u8 n )
   if s" yes" else s" no" then ;

: RB-FFIX6 ( r -- )
   fdup 0.0 f< if 45 RB-C fnegate then
   1000000.0 f* 0.5 f+ f>s {: scaled:n :}
   scaled 1000000 / RB# 46 RB-C
   scaled 1000000 mod {: x:n :}
   x 100000 / 48 + RB-C x 100000 mod 10000 / 48 + RB-C x 10000 mod 1000 / 48 + RB-C
   x 1000 mod 100 / 48 + RB-C x 100 mod 10 / 48 + RB-C x 10 mod 48 + RB-C ;

: CSV-VEC ( n -- )
   {: ix:n :}
   RG-HAVE-T ix A@ drop
   91 RB-C RG-TX ix FA@ RB-FFIX6 59 RB-C RG-TY ix FA@ RB-FFIX6 59 RB-C RG-TZ ix FA@ RB-FFIX6 93 RB-C ;

: CSV-ROT ( n -- )
   {: ix:n :}
   91 RB-C RG-RX ix FA@ RB-FFIX6 59 RB-C RG-RY ix FA@ RB-FFIX6 59 RB-C RG-RZ ix FA@ RB-FFIX6 93 RB-C ;

: MD-VEC ( ptr a ptr a ptr a n -- )
   {: x:ptr y:ptr z:ptr ix:n :}
   96 RB-C 91 RB-C x ix FA@ RB-FFIX6 s" , " RB+ y ix FA@ RB-FFIX6 s" , " RB+ z ix FA@ RB-FFIX6 93 RB-C 96 RB-C ;

: MD-TRANS ( n -- )
   {: ix:n :}
   RG-HAVE-T ix A@ 0= if s" missing" RB+ exit then
   RG-TX RG-TY RG-TZ ix MD-VEC ;

: MD-ROT ( n -- )
   {: ix:n :}
   RG-HAVE-R ix A@ 0= if s" missing" RB+ exit then
   RG-RX RG-RY RG-RZ ix MD-VEC ;

: OPT-RES ( n -- )
   dup RG-HAVE-RES swap A@ 0= if drop exit then
   RG-RES swap FA@ RB-FFIX6 ;

: READINESS-CSV$ ( -- ptr u8 n )
   RB-RESET
   s" scope,name,status,translation_m,rotation_rpy_deg,residual,residual_required,ready,reason" RB+ RB-NL
   0 RG-I !
   begin RG-I @ RG-ROWS @ < while
      RG-I @ SCOPE$ RB+ CM RG-I @ NAME$ RB+ CM RG-I @ STATUS$ RB+ CM
      RG-I @ RG-HAVE-T swap A@ 0 <> if RG-I @ CSV-VEC then CM
      RG-I @ RG-HAVE-R swap A@ 0 <> if RG-I @ CSV-ROT then CM
      RG-I @ OPT-RES CM
      RG-RES-REQ RG-I @ A@ 0 <> YESNO RB+ CM
      RG-READY RG-I @ A@ 0 <> YESNO RB+ CM
      RG-I @ REASON$ RB+ RB-NL
      RG-I @ 1+ RG-I !
   repeat
   RB$ ;

: READINESS-MD$ ( ptr u8 n -- ptr u8 n )
   {: source:ptr sourceu:n :}
   RB-RESET
   s" # Rig Geometry Readiness" RB+ RB-NL RB-NL
   s" - source: " RB+ source sourceu RB+ RB-NL
   s" - schema: odin.rig_geometry.extrinsics_initial.v1" RB+ RB-NL
   s" - schema ready: " RB+ RG-SCHEMA-READY @ 0 <> YESNO RB+ RB-NL
   s" - cameras: " RB+ RG-CAMERAS @ RB# RB-NL
   s" - camera count ready: " RB+ RG-CAMERA-COUNT-READY @ 0 <> YESNO RB+ RB-NL
   s" - duplicate logical names: " RB+ RG-DUP-NAMES @ RB# RB-NL
   s" - ready camera transforms: " RB+ RG-READY-CAMS @ RB# 47 RB-C RG-CAMERAS @ RB# RB-NL
   s" - ready transforms: " RB+ RG-READY-XFORMS @ RB# 47 RB-C RG-ROWS @ RB# RB-NL
   s" - result: " RB+ RESULT$ RB+ RB-NL RB-NL
   s" | scope | name | status | translation | rotation | residual | ready | reason |" RB+ RB-NL
   s" | --- | --- | --- | --- | --- | ---: | --- | --- |" RB+ RB-NL
   0 RG-I !
   begin RG-I @ RG-ROWS @ < while
      s" | " RB+ RG-I @ SCOPE$ RB+ s"  | " RB+ RG-I @ NAME$ RB+ s"  | " RB+ RG-I @ STATUS$ RB+
      s"  | " RB+ RG-I @ MD-TRANS s"  | " RB+ RG-I @ MD-ROT s"  | " RB+ RG-I @ OPT-RES
      s"  | " RB+ RG-READY RG-I @ A@ 0 <> YESNO RB+ s"  | " RB+ RG-I @ REASON$ RB+ s"  |" RB+ RB-NL
      RG-I @ 1+ RG-I !
   repeat
   RB$ ;

: RUN-READINESS ( -- n )
   EXTRINSICS$ RG-READ-BUF RG-READ-CAP READ-ALL RG-READ-U !
   RG-READ-BUF RG-READ-U @ PARSE-EXTRINSICS
   OUTPUT$ MAKE-DIRS
   s" readiness.csv" PATH-IN-OUT$ READINESS-CSV$ WRITE-ALL
   s" summary.md" PATH-IN-OUT$ EXTRINSICS$ READINESS-MD$ WRITE-ALL
   RESULT$ s" pass" STR= if 0 else 1 then ;

: FIELD-A! ( ptr u8 n n -- )
   {: a:ptr u:n ix:n :}
   a RG-FIELD-A ix PTRA!
   u RG-FIELD-U ix A! ;

: FIELD$ ( n -- ptr u8 n )
   {: ix:n :}
   RG-FIELD-A ix PTRA@
   RG-FIELD-U ix A@ ;

: TRIM-SPAN ( ptr u8 n -- ptr u8 n )
   TRIM ;

: SPLIT-FIELDS ( ptr u8 n -- )
   {: line:ptr lineu:n :}
   0 RG-I ! 0 RG-J !
   line RG-LINE-A ! lineu RG-LINE-U !
   0 RG-K !
   begin RG-J @ lineu <= while
      RG-J @ lineu = if
         line RG-K @ + RG-J @ RG-K @ - TRIM-SPAN RG-I @ FIELD-A!
         RG-I @ 1+ RG-I !
         RG-J @ 1+ RG-J !
      else
         line RG-J @ + c@ 44 = if
            line RG-K @ + RG-J @ RG-K @ - TRIM-SPAN RG-I @ FIELD-A!
            RG-I @ 1+ RG-I !
            RG-J @ 1+ RG-K !
         then
         RG-J @ 1+ RG-J !
      then
   repeat
   RG-I @ RG-SURVEY-FIELDS <> if E-RG-SURVEY throw then ;

: TOKEN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0 <= if RG-FALSE exit then
   0 RG-I !
   begin RG-I @ u < while
      a RG-I @ + c@ {: c:n :}
      c 48 >= c 57 <= and c 65 >= c 90 <= and or c 97 >= c 122 <= and or
      c 95 = or c 45 = or c 46 = or c 58 = or 0= if RG-FALSE exit then
      RG-I @ 1+ RG-I !
   repeat
   RG-TRUE ;

: SURVEY-HEADER? ( ptr u8 n -- bool )
   s" kind,logical_name,status,tx_m,ty_m,tz_m,roll_deg,pitch_deg,yaw_deg,residual,role" STR= ;

: LINE-SKIP? ( ptr u8 n -- bool )
   TRIM-SPAN dup 0= if 2drop RG-TRUE exit then
   drop c@ 35 = ;

: SURVEY-SOURCE! ( ptr u8 n -- )
   RG-SRC-U ! SRC-A! ;

: SURVEY-LINE-END ( n -- n )
   RG-J !
   begin RG-J @ RG-SRC-U @ < while
      SRC-A@ RG-J @ + c@ 10 = if RG-J @ exit then
      RG-J @ 1+ RG-J !
   repeat
   RG-J @ ;

: SURVEY-NEXT-LINE ( n -- n )
   dup RG-SRC-U @ < if 1+ then ;

: SURVEY-LINE$ ( n -- ptr u8 n n )
   {: start:n :}
   start SURVEY-LINE-END {: end:n :}
   SRC-A@ start + end start - TRIM-SPAN
   end SURVEY-NEXT-LINE ;

: JSON-NULL ( -- )
   s" null" RB+ ;

: JSON-FIELD-STR ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n :}
   key keyu QK val valu QSTR ;

: FIELD-EMPTY? ( n -- bool )
   FIELD$ nip 0= ;

: JSON-VEC-FIELDS ( n n n -- )
   {: x:n y:n z:n :}
   0 x FIELD-EMPTY? if 1+ then
   y FIELD-EMPTY? if 1+ then
   z FIELD-EMPTY? if 1+ then {: blanks:n :}
   blanks 3 = if JSON-NULL exit then
   blanks 0 <> if E-RG-SURVEY throw then
   91 RB-C x FIELD$ RB+ CM y FIELD$ RB+ CM z FIELD$ RB+ 93 RB-C ;

: JSON-FIELD-VEC3 ( ptr u8 n n n n -- )
   {: key:ptr keyu:n x:n y:n z:n :}
   key keyu QK x y z JSON-VEC-FIELDS ;

: JSON-OPT-NUM ( ptr u8 n -- )
   dup 0= if 2drop JSON-NULL else RB+ then ;

: JSON-FIELD-OPT-NUM ( ptr u8 n n -- )
   {: key:ptr keyu:n ix:n :}
   key keyu QK ix FIELD$ JSON-OPT-NUM ;

: SURVEY-TRANSFORM ( -- )
   123 RB-C
   s" status" 2 FIELD$ JSON-FIELD-STR CM
   s" translation_m" 3 4 5 JSON-FIELD-VEC3 CM
   s" rotation_rpy_deg" 6 7 8 JSON-FIELD-VEC3 CM
   s" residual" 9 JSON-FIELD-OPT-NUM
   125 RB-C ;

: SURVEY-ROW-VALIDATE ( -- )
   2 FIELD$ s" measured" STR= 2 FIELD$ s" estimated" STR= or 0= if E-RG-SURVEY throw then
   0 FIELD$ s" rig_to_truck" STR= if
      1 FIELD$ nip 0 <> if E-RG-SURVEY throw then
      1 RG-SAW-RIG !
      exit
   then
   0 FIELD$ s" camera" STR= if
      1 FIELD$ TOKEN? 0= if E-RG-SURVEY throw then
      RG-SURVEY-CAMS @ 1+ RG-SURVEY-CAMS !
      exit
   then
   E-RG-SURVEY throw ;

: SURVEY-RIG-JSON-LINE ( -- )
   SURVEY-ROW-VALIDATE
   0 FIELD$ s" rig_to_truck" STR= if
      s"   " RB+ s" rig_to_truck" QK SURVEY-TRANSFORM CM RB-NL exit
   then
   E-RG-SURVEY throw ;

: SURVEY-CAMERA-JSON-LINE ( -- )
   SURVEY-ROW-VALIDATE
   0 FIELD$ s" camera" STR= 0= if E-RG-SURVEY throw then
   RG-SURVEY-CAMS @ 1 > if CM RB-NL then
   s"     {" RB+ RB-NL
   s"       " RB+ s" logical_name" QK 1 FIELD$ QSTR CM RB-NL
   10 FIELD$ nip 0 > if s"       " RB+ s" role" QK 10 FIELD$ QSTR CM RB-NL then
   s"       " RB+ s" camera_to_rig" QK SURVEY-TRANSFORM RB-NL
   s"     }" RB+ ;

: SURVEY-EMIT-PREAMBLE ( -- )
   s" {" RB+ RB-NL
   s"   " RB+ s" schema_version" QK s" odin.rig_geometry.extrinsics_initial.v1" QSTR CM RB-NL ;

: SURVEY-RIG-PASS-LINE ( ptr u8 n -- )
   2dup LINE-SKIP? if 2drop exit then
   RG-SAW-HEADER @ 0= if
      SURVEY-HEADER? 0= if E-RG-SURVEY throw then
      1 RG-SAW-HEADER !
      exit
   then
   SPLIT-FIELDS
   0 FIELD$ s" rig_to_truck" STR= if SURVEY-RIG-JSON-LINE then ;

: SURVEY-CAMERA-PASS-LINE ( ptr u8 n -- )
   2dup LINE-SKIP? if 2drop exit then
   2dup SURVEY-HEADER? if 2drop exit then
   SPLIT-FIELDS
   0 FIELD$ s" camera" STR= if SURVEY-CAMERA-JSON-LINE then ;

: SURVEY-RIG-PASS ( -- )
   0 RG-I !
   begin RG-I @ RG-SRC-U @ < while
      RG-I @ SURVEY-LINE$ {: line:ptr lineu:n next:n :}
      line lineu SURVEY-RIG-PASS-LINE
      next RG-I !
   repeat
   RG-SAW-HEADER @ 0= if E-RG-SURVEY throw then
   RG-SAW-RIG @ 0= if E-RG-SURVEY throw then ;

: SURVEY-CAMERA-PASS ( -- )
   s"   " RB+ s" cameras" QK 91 RB-C RB-NL
   0 RG-I !
   begin RG-I @ RG-SRC-U @ < while
      RG-I @ SURVEY-LINE$ {: line:ptr lineu:n next:n :}
      line lineu SURVEY-CAMERA-PASS-LINE
      next RG-I !
   repeat
   RB-NL s"   ]" RB+ RB-NL s" }" RB+ RB-NL
   RG-SURVEY-CAMS @ RG-MAX-CAMERAS <> if E-RG-SURVEY throw then ;

: SURVEY-JSON$ ( ptr u8 n -- ptr u8 n )
   SURVEY-SOURCE!
   0 RG-SAW-HEADER ! 0 RG-SAW-RIG ! 0 RG-SURVEY-CAMS !
   RB-RESET
   SURVEY-EMIT-PREAMBLE
   SURVEY-RIG-PASS
   SURVEY-CAMERA-PASS
   RB$ ;

: RUN-SURVEY ( -- n )
   SURVEY$ RG-READ-BUF RG-READ-CAP READ-ALL RG-READ-U !
   OUTPUT$ MAKE-DIRS
   s" extrinsics.json" PATH-IN-OUT$ RG-READ-BUF RG-READ-U @ SURVEY-JSON$ 2dup WRITE-ALL
   PARSE-EXTRINSICS
   s" readiness.csv" PATH-IN-OUT$ READINESS-CSV$ WRITE-ALL
   s" summary.md" PATH-IN-OUT$ s" extrinsics.json" PATH-IN-OUT$ READINESS-MD$ WRITE-ALL
   RESULT$ s" pass" STR= if 0 else 1 then ;

public

: RESET ( -- ) RG-RESET ;
: EXTRINSICS! ( ptr u8 n -- ) SET-EXTRINSICS ;
: SURVEY! ( ptr u8 n -- ) SET-SURVEY ;
: OUTPUT! ( ptr u8 n -- ) SET-OUTPUT ;
: REQUIRE-READY! ( -- ) 1 RG-REQUIRE-READY ! ;
: PARSE ( ptr u8 n -- ) PARSE-EXTRINSICS ;
: READINESS-CSV ( -- ptr u8 n ) READINESS-CSV$ ;
: READINESS-MD ( ptr u8 n -- ptr u8 n ) READINESS-MD$ ;
: SURVEY-JSON ( ptr u8 n -- ptr u8 n ) SURVEY-JSON$ ;
: RUN-GEOMETRY ( -- n ) RUN-READINESS ;
: RUN-SURVEY ( -- n ) RUN-SURVEY ;
: RESULT ( -- ptr u8 n ) RESULT$ ;

end-package
