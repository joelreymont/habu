\ build-fixpoint.f - checked self-rebuild orchestration.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and lib/codesign.f.
\ The stamp key uses the baked SHA256 words; no lib/content-key.f dependency.

262144 constant BF-SOURCE-CAP
32768 constant BF-CMP-CAP
4 constant BF-MAX-GENS
10 constant BF-LF
64 constant BF-USAGE-RC
74 constant BF-BUILD-RC
32 constant BF-SP
34 constant BF-DQ
$2F constant BF-SLASH
64 constant BF-STAMP-HEX-U
12 constant BF-STAMP-PREFIX-U
32 constant BF-STAMP-DG-U
256 constant BF-STAMP-CAP

create BF-LF-BUF 1 allot
create BF-CHAR-BUF 1 allot
create BF-STAMP-KEY 80 allot
create BF-STAMP-OLD 80 allot
create BF-STAMP-DG 40 allot
create BF-REC-STAGE-DG 40 allot
create BF-REC-STDIN-DG 40 allot
create BF-STAMP-BUF BF-STAMP-CAP allot
create BF-STAMP-PATH-BUF FS-PATH-CAP allot
create BF-STAMP-DIR-BUF FS-PATH-CAP allot
create BF-STAMP-DEF-BUF FS-PATH-CAP allot
create BF-ENGINE-BUF FS-PATH-CAP allot
BF-LF BF-LF-BUF c!

variable BF-ART-PATH-A
variable BF-OUT-PATH-A
variable BF-A-PATH-A
variable BF-B-PATH-A
variable BF-SOURCE-BUF-A
variable BF-CMP-A-BUF-A
variable BF-CMP-B-BUF-A
variable BF-SOURCE-LEN
variable BF-A-LEN
variable BF-B-LEN
variable BF-FDA
variable BF-FDB
variable BF-APP-IN
variable BF-APP-OUT
variable BF-RA
variable BF-RB
variable BF-APP-RD
variable BF-APP-WR
variable BF-APP-OFF
variable BF-GEN
variable BF-FOUND
variable BF-PID
variable BF-TMP-A
variable BF-TMP-U
variable BF-STRIP-R
variable BF-STRIP-W
variable BF-STRIP-OFF
variable BF-STAMP-PATH-U
variable BF-STAMP-DIR-U
variable BF-STAMP-DEF-U
variable BF-STAMP-U
variable BF-REC-STAGE?
variable BF-REC-STDIN?
variable BF-ENGINE-U
variable BF-FORCE

: BF-TMP-A-FIELD ( -- ptr ptr u8 )
   BF-TMP-A 0 ptr-field ;

: BF-TMP-A@ ( -- ptr u8 )
   BF-TMP-A-FIELD @ ;

: BF-TMP-A! ( ptr u8 -- )
   BF-TMP-A-FIELD ! ;

: BF-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BF-PTR-U8@ ( ptr a -- ptr u8 )
   BF-PTR-U8-FIELD @ ;

: BF-PTR-U8! ( ptr u8 ptr a -- )
   BF-PTR-U8-FIELD ! ;

: BF-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: BF-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap BF-ALLOC-BUF slot BF-PTR-U8! then
   slot BF-PTR-U8@ ;

: BF-SOURCE-BUF ( -- ptr u8 )
   BF-SOURCE-BUF-A BF-SOURCE-CAP BF-BUF ;

: BF-CMP-A ( -- ptr u8 )
   BF-CMP-A-BUF-A BF-CMP-CAP BF-BUF ;

: BF-CMP-B ( -- ptr u8 )
   BF-CMP-B-BUF-A BF-CMP-CAP BF-BUF ;

: BF-PATH-BUF ( ptr a -- ptr u8 )
   FS-PATH-CAP BF-BUF ;

: BF-ART-PATH ( -- ptr u8 )
   BF-ART-PATH-A BF-PATH-BUF ;

: BF-OUT-PATH ( -- ptr u8 )
   BF-OUT-PATH-A BF-PATH-BUF ;

: BF-A-PATH ( -- ptr u8 )
   BF-A-PATH-A BF-PATH-BUF ;

: BF-B-PATH ( -- ptr u8 )
   BF-B-PATH-A BF-PATH-BUF ;

: BF-TRUE ( -- bool )
   0 0= ;

: BF-FALSE ( -- bool )
   0 0= 0= ;

: BF-TMP! ( ptr u8 n -- )
   {: a:ptr u :}
   u BF-TMP-U !
   a BF-TMP-A! ;

: BF-TMP-OVERRIDE$ ( -- ptr u8 n )
   BF-TMP-A@ BF-TMP-U @ ;

: BF-TMP-RESET ( -- )
   0 BF-TMP-U ! ;

: BF-TMP$ ( -- ptr u8 n )
   BF-TMP-U @ 0 > if BF-TMP-OVERRIDE$ exit then
   s" HB_TMP" GETENV dup 0= if drop drop s" /tmp" then ;

: BF-TMP> ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu dst:ptr :}
   BF-TMP$ {: root:ptr rootu :}
   rootu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu dst JOIN-PATH ;

: BF-ART$ ( ptr u8 n -- ptr u8 n )
   BF-ART-PATH BF-TMP> BF-ART-PATH swap ;

: BF-OUT$ ( ptr u8 n -- ptr u8 n )
   BF-OUT-PATH BF-TMP> BF-OUT-PATH swap ;

: BF-A$ ( ptr u8 n -- ptr u8 n )
   BF-A-PATH BF-TMP> BF-A-LEN !
   BF-A-PATH BF-A-LEN @ ;

: BF-B$ ( ptr u8 n -- ptr u8 n )
   BF-B-PATH BF-TMP> BF-B-LEN !
   BF-B-PATH BF-B-LEN @ ;

: BF-EXPECT-PATH ( ptr u8 n -- ) {: path:ptr pathu :}
   pathu 0 <= if E-BUILD-PATH throw then
   path pathu FILE? 0= if E-BUILD-PATH throw then ;

: BF-EXPECT ( ptr u8 n -- )
   BF-ART$ BF-EXPECT-PATH ;

: BF-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: BF-REMOVE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: BF-RENAME-TMP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu BF-A$ dst dstu BF-B$ RENAME-FILE ;

: BF-CHMOD-X-TMP ( ptr u8 n -- )
   BF-A$ CHMOD-X ;

: BF-OPEN-INPUT ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if E-BUILD-PATH throw then ;

: BF-PREPARE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN BF-TMP$ >LEN PROC-ENV+ ;

: BF-FINISH-PID ( pid -- n ) {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid PROC-WAIT-RC RC>N ;

: BF-RUN-ENV-FDS ( ptr u8 n n n n -- n ) {: exe:ptr exeu infd outfd errfd :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD outfd >FD errfd >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-PREPARE-STAGE-ARGV ( ptr u8 n -- ptr u8 ptr a )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   >LEN PROC-ARGV-PREPARE ;

: BF-PREPARE-LOAD-STAGE-ARGV ( ptr u8 n ptr u8 n -- ptr u8 ptr a ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   exe exeu >LEN PROC-ARGV-PREPARE ;

: BF-RUN-STAGE-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   exe exeu BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-RUN-LOAD-STAGE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   BF-PREPARE-ENV
   exe exeu src srcu BF-PREPARE-LOAD-STAGE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-EXE ( ptr u8 n -- n )
   -1 -1 -1 BF-RUN-ENV-FDS ;

: BF-RUN-STAGE-ENV-EXE ( ptr u8 n -- n )
   BF-PREPARE-ENV
   BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-RUN-STAGE-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-STAGE-ENV-INFD ;

: BF-RUN-ENV-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-ENV-EXE ;

: BF-RUN-STAGE-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-STAGE-ENV-EXE ;

: BF-RUN-ENV-TMP-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu BF-A$ src srcu BF-B$ BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-CODESIGN-VERIFY-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN-VERIFY ;

: BF-CODESIGN-FORCE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN-FORCE ;

: BF-RESET-OUT ( ptr u8 n -- )
   BF-OUT$ BF-SOURCE-BUF 0 WRITE-ALL ;

: BF-APPEND-BYTES ( ptr u8 n ptr u8 n -- ) {: out:ptr outu a:ptr u :}
   out outu BF-OUT$ a u APPEND-FILE ;

: BF-APPEND-LF ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-OUT$ BF-LF-BUF 1 APPEND-FILE ;

: BF-APPEND-C ( ptr u8 n n -- ) {: out:ptr outu:n c:n :}
   c BF-CHAR-BUF c!
   out outu BF-OUT$ BF-CHAR-BUF 1 APPEND-FILE ;

: BF-APPEND-LINE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n a:ptr u:n :}
   out outu a u BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-HIDE-CALL ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n name:ptr nameu:n word:ptr wordu:n :}
   out outu s" s" BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu name nameu BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu word wordu BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-HIDE2-CALL ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n a:ptr au:n b:ptr bu:n word:ptr wordu:n :}
   out outu s" s" BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu a au BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu s" s" BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu b bu BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu word wordu BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-STAGE2-HIDE-DEFS ( ptr u8 n -- )
   2dup s" BFR-USIGS-RESET" BF-APPEND-LINE
   s" SEQ" s" T-CON" s" BFR-HIDE-DICT-FROM-EARLIEST" BF-APPEND-HIDE2-CALL ;

: BF-APP-CLOSE ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if close else drop then
   -1 p ! ;

: BF-APP-THROW ( n -- )
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE
   throw ;

: BF-APP-RESET ( -- )
   -1 BF-APP-IN !
   -1 BF-APP-OUT ! ;

: BF-APP-OPEN ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu FS-PATHZ open-rd BF-APP-IN !
   BF-APP-IN @ 0 < if E-FS-OPEN BF-APP-THROW then
   dst dstu OPEN-APPEND-FD BF-APP-OUT !
   BF-APP-OUT @ 0 < if E-FS-OPEN BF-APP-THROW then ;

: BF-APP-WRITE-CHUNK ( n -- ) {: u :}
   0 BF-APP-OFF !
   begin BF-APP-OFF @ u < while
      BF-APP-OUT @ BF-SOURCE-BUF BF-APP-OFF @ + u BF-APP-OFF @ - write BF-APP-WR !
      BF-APP-WR @ 0 <= if E-FS-IO BF-APP-THROW then
      BF-APP-WR @ u BF-APP-OFF @ - > if E-FS-IO BF-APP-THROW then
      BF-APP-OFF @ BF-APP-WR @ + BF-APP-OFF !
   repeat ;

: BF-APPEND-FILE-STREAM ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   BF-APP-RESET
   src srcu dst dstu BF-APP-OPEN
   begin
      BF-APP-IN @ BF-SOURCE-BUF BF-SOURCE-CAP read BF-APP-RD !
      BF-APP-RD @ 0 < if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ BF-SOURCE-CAP > if E-FS-IO BF-APP-THROW then
      BF-APP-RD @ 0 >
   while
      BF-APP-RD @ BF-APP-WRITE-CHUNK
   repeat
   BF-APP-IN BF-APP-CLOSE
   BF-APP-OUT BF-APP-CLOSE ;

: BF-APPEND-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu src:ptr srcu :}
   src srcu out outu BF-OUT$ BF-APPEND-FILE-STREAM
   out outu BF-APPEND-LF ;

: BF-READ-SOURCE ( ptr u8 n -- )
   BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN ! ;

: BF-SOURCE-HAS? ( ptr u8 n -- bool )
   BF-SOURCE-BUF BF-SOURCE-LEN @ 2swap CONTAINS? ;

: BF-SOURCE-FIND ( ptr u8 n -- n )
   BF-SOURCE-BUF BF-SOURCE-LEN @ 2swap FIND-SUB ;

: BF-SOURCE-REQUIRE ( n -- n )
   dup 0 >= if exit then
   s" build-fixpoint: source split marker missing" BF-BUILD-RC die ;

: BF-SRC-C@ ( n -- u8 )
   BF-SOURCE-BUF + c@ ;

: BF-SRC-C! ( u8 n -- )
   BF-SOURCE-BUF + c! ;

: BF-STRIP-WRITE ( u8 -- )
   BF-STRIP-W @ BF-SRC-C!
   BF-STRIP-W @ 1+ BF-STRIP-W ! ;

: BF-STRIP-R++ ( -- )
   BF-STRIP-R @ 1+ BF-STRIP-R ! ;

: BF-STRIP-SKIP-PAREN ( -- )
   BF-STRIP-R++
   begin BF-STRIP-R @ BF-STRIP-OFF @ < while
      BF-STRIP-R @ BF-SRC-C@ 41 = if BF-STRIP-R++ exit then
      BF-STRIP-R++
   repeat ;

: BF-STRIP-KEEP ( -- )
   BF-STRIP-R @ BF-SRC-C@ BF-STRIP-WRITE
   BF-STRIP-R++ ;

: BF-STRIP-RANGE ( n -- n )
   BF-STRIP-OFF !
   0 BF-STRIP-R !
   0 BF-STRIP-W !
   begin BF-STRIP-R @ BF-STRIP-OFF @ < while
      BF-STRIP-R @ BF-SRC-C@ 40 = if
         BF-STRIP-SKIP-PAREN
      else
         BF-STRIP-KEEP
      then
   repeat
   BF-STRIP-W @ ;

: BF-APPEND-SOURCE-BEFORE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE {: off:n :}
   out outu BF-SOURCE-BUF off BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-SOURCE-BEFORE-STRIPPED ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE BF-STRIP-RANGE {: kept:n :}
   out outu BF-SOURCE-BUF kept BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-SOURCE-FROM ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n src:ptr srcu:n mark:ptr marku:n :}
   src srcu BF-READ-SOURCE
   mark marku BF-SOURCE-FIND BF-SOURCE-REQUIRE {: off:n :}
   out outu BF-SOURCE-BUF off + BF-SOURCE-LEN @ off - BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-SOURCE-MUST-HAVE ( ptr u8 n -- )
   BF-SOURCE-HAS? 0= if s" build-fixpoint: native emitter shape missing" BF-BUILD-RC die then ;

: BF-SOURCE-MUST-LACK ( ptr u8 n -- )
   BF-SOURCE-HAS? if s" build-fixpoint: unsafe native emitter shape" BF-BUILD-RC die then ;

: BF-PREFLIGHT-HABU2 ( -- )
   s" src/habu/habu2.f" BF-READ-SOURCE
   s" variable CLOC-MAIN  variable CLOC-NOT" BF-SOURCE-MUST-HAVE
   s" variable CLOC-MEM   variable CLOC-QOK" BF-SOURCE-MUST-HAVE
   s" : C-LOCAL-REF-ARGS ( label label -- )" BF-SOURCE-MUST-HAVE
   s" : C-LOCAL-REF-LABELS ( -- )" BF-SOURCE-MUST-HAVE
   s" : EMIT-RESET-BUILDER ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" {: lmainlbl notloc :}" BF-SOURCE-MUST-LACK
   s" LBL LBL {: lmem qlrefok :}" BF-SOURCE-MUST-LACK
   s" {: a:ptr u :}" BF-SOURCE-MUST-LACK
   s" CLOC-MAIN LABEL@ B," BF-SOURCE-MUST-HAVE
   s" CLOC-MAIN @ B ;" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT-ICODE ( -- )
   s" src/arch/arm64/icode.f" BF-READ-SOURCE
   s" variable CODE-A" BF-SOURCE-MUST-HAVE
   s" : CODE ( -- ptr u8 )" BF-SOURCE-MUST-HAVE
   s" : ICODE-TABS ( -- ptr n )" BF-SOURCE-MUST-HAVE
   s" icode: code mmap failed" BF-SOURCE-MUST-HAVE
   s" icode: table mmap failed" BF-SOURCE-MUST-HAVE
   s" variable BYA" BF-SOURCE-MUST-HAVE
   s" variable BYU" BF-SOURCE-MUST-HAVE
   s" : BYA@ ( -- ptr u8 )" BF-SOURCE-MUST-HAVE
   s" : BYTES-ARGS ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-CAP ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-COPY ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-PAD ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES, ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" create CODE CODE-CAP-BYTES allot" BF-SOURCE-MUST-LACK
   s" create LBLP LBL-CAP cells allot" BF-SOURCE-MUST-LACK
   s" create FXS 2048 cells allot" BF-SOURCE-MUST-LACK
   s" {: a:ptr u :}" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT-HABU1 ( -- )
   s" src/habu/habu1.f" BF-READ-SOURCE
   s" variable PR-A  variable PR-U  variable PR-L  variable PR-E" BF-SOURCE-MUST-HAVE
   s" variable FP-A  variable FP-U  variable FP-XT" BF-SOURCE-MUST-HAVE
   s" variable SDA-FD  variable SDA-NEW  variable SDA-SKIP" BF-SOURCE-MUST-HAVE
   s" variable BSP-OK  variable BSP-DN  variable BSP-SAD" BF-SOURCE-MUST-HAVE
   s" variable SZA-I" BF-SOURCE-MUST-HAVE
   s" : REG-PRIM ( ptr u8 n n n -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM ( ptr u8 n n -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM-L ( ptr u8 n n -- )" BF-SOURCE-MUST-HAVE
   s" : PR-COPY-NAME ( -- )" BF-SOURCE-MUST-HAVE
   s" : BSP-LABELS3 ( -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM {: na:ptr nu xt :}" BF-SOURCE-MUST-LACK
   s" : FPRIM-L {: na:ptr nu xt :}" BF-SOURCE-MUST-LACK
   s" : REG-PRIM {: na:ptr nu lbl elbl :}" BF-SOURCE-MUST-LACK
   s" : ?PRIM-SPACE {: na:ptr nu :}" BF-SOURCE-MUST-LACK
   s" : SPAWN-DUP2-ACTION ( reg fd -- )" BF-SOURCE-MUST-HAVE
   s" : SPAWN-CHDIR-ACTION ( reg label -- )" BF-SOURCE-MUST-HAVE
   s" : SPAWN-DUP2-ACTION ( n n -- ) {: fdreg newfd :}" BF-SOURCE-MUST-LACK
   s" : SPAWN-CHDIR-ACTION ( n n -- ) {: cwdreg fail :}" BF-SOURCE-MUST-LACK
   s" 14 SP SPAWN-ADESC-OFF SZA-I @ + STR," BF-SOURCE-MUST-HAVE
   s" 14 SP SPAWN-ADESC-OFF + over + STR," BF-SOURCE-MUST-LACK
   s" LBL LBL LBL {: spok spdn sad :}" BF-SOURCE-MUST-LACK
   s" LBL LBL {: spok spdn :}" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT ( -- )
   BF-PREFLIGHT-HABU2
   BF-PREFLIGHT-HABU1
   BF-PREFLIGHT-ICODE ;

: BF-TARGET-UNKNOWN ( -- )
   s" build-fixpoint: unknown target" BF-BUILD-RC die ;

: BF-APPEND-TARGET-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/layout.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/layout.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-SYS ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/sys.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/sys.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-FLAG ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/target.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/target.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-IMAGE-BYTES ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/image-bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-TARGET-IMAGE ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/elf.f" BF-APPEND-SOURCE
      out outu s" src/os/linux/sign.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/macho.f" BF-APPEND-SOURCE
      out outu s" src/os/macos/sign2.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-ROLES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/roles.f" BF-APPEND-SOURCE ;

: BF-APPEND-CHECKER-BOOT ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/structures.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE
   out outu s" src/core/render.f" BF-APPEND-SOURCE
   out outu s" src/core/check-hook.f" BF-APPEND-SOURCE
   out outu s" src/core/structures-effects.f" BF-APPEND-SOURCE ;

: BF-APPEND-CORE-BYTES ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" src/core/bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-CHECK-OFF ( ptr u8 n -- )
   s" 0 set-check" BF-APPEND-LINE ;

: BF-APPEND-FRESH-CHECK-HOOK ( ptr u8 n -- )
   s" ' HOOK set-check" BF-APPEND-LINE ;

: BF-APPEND-SQUOTE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n a:ptr u:n :}
   out outu s" s" BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C
   out outu BF-SP BF-APPEND-C
   out outu a u BF-APPEND-BYTES
   out outu BF-DQ BF-APPEND-C ;

: BF-APPEND-TRUST ( ptr u8 n ptr u8 n ptr u8 n -- ) {: out:ptr outu:n name:ptr nameu:n sig:ptr sigu:n :}
   out outu name nameu BF-APPEND-SQUOTE
   out outu BF-SP BF-APPEND-C
   out outu sig sigu BF-APPEND-SQUOTE
   out outu s"  TRUST" BF-APPEND-LINE ;

: BF-APPEND-IMAGE-TRUSTS ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu s" ASM-CODE" s" -- asm" BF-APPEND-TRUST
   out outu s" BUILD-IMAGE" s" asm -- img" BF-APPEND-TRUST
   out outu s" BUILD-SNAP-HDR" s" n -- snap n" BF-APPEND-TRUST
   out outu s" SET-SIGID" s" ptr u8 n --" BF-APPEND-TRUST
   out outu s" CODESIG2" s" img -- img" BF-APPEND-TRUST ;

: BF-APPEND-HABU-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/layout.f" BF-APPEND-SOURCE ;

: BF-APPEND-ENV-BASE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/env-base.f" BF-APPEND-SOURCE ;

: BF-APPEND-SCRIPT-ARGV ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/script-argv.f" BF-APPEND-SOURCE ;

: BF-APPEND-COMBINATORS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/combinators.f" BF-APPEND-SOURCE ;

: BF-APPEND-EXEC-VECTOR ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/exec-vector.f" BF-APPEND-SOURCE ;

: BF-APPEND-INCLUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/include.f" BF-APPEND-SOURCE ;

: BF-APPEND-ENUMS ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/enums.f" BF-APPEND-SOURCE ;

: BF-APPEND-COMMON ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-APPEND-ROLES
   out outu BF-APPEND-CORE-BYTES
   out outu BF-APPEND-TARGET-FLAG
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-LAYOUT
   out outu BF-APPEND-TARGET-SYS
   out outu BF-APPEND-HABU-LAYOUT
   out outu BF-APPEND-ENV-BASE
   out outu BF-APPEND-SCRIPT-ARGV
   out outu BF-APPEND-ENUMS
   out outu BF-APPEND-EXEC-VECTOR
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu BF-APPEND-COMBINATORS
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-CHECK-OFF
   out outu BF-APPEND-TARGET-IMAGE
   out outu BF-APPEND-FRESH-CHECK-HOOK
   out outu BF-APPEND-IMAGE-TRUSTS
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE
   out outu s" src/habu/xref.f" BF-APPEND-SOURCE ;

: BF-APPEND-DRIVER-IO ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/driver-io.f" BF-APPEND-SOURCE ;

: BF-APPEND-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/hide.f" BF-APPEND-SOURCE
   out outu s" BFR-CHECK-OFF" BF-APPEND-LINE
   out outu BF-STAGE2-HIDE-DEFS
   out outu BF-APPEND-CHECKER-BOOT ;

: BF-APPEND-STDIN-RUN-PRELUDE ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-APPEND-RUN-PRELUDE ;

: BF-EMIT-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-EMIT-STDIN-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-STDIN-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu BF-APPEND-INCLUDE
   out outu BF-APPEND-DRIVER-IO
   out outu driver driveru BF-APPEND-SOURCE ;

\ Snapshot source layout: the dev-engine keep surface (the same files the
\ plain engine bakes as its startup prefix, plus the baked REPL sources)
\ loads FIRST, then SNAP-TAIL-MARK opens the builder-only tail. snap.f
\ retires everything from the marker before SNAPGO, so the persisted image
\ carries only the keep surface.
: BF-APPEND-SNAP-KEEP ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-ROLES
   out outu BF-APPEND-CORE-BYTES
   out outu BF-APPEND-TARGET-FLAG
   out outu BF-APPEND-TARGET-LAYOUT
   out outu BF-APPEND-HABU-LAYOUT
   out outu BF-APPEND-ENV-BASE
   out outu BF-APPEND-INCLUDE
   out outu BF-APPEND-ENUMS
   out outu BF-APPEND-EXEC-VECTOR
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu BF-APPEND-COMBINATORS
   out outu s" src/habu/xref.f" BF-APPEND-SOURCE
   out outu BF-APPEND-SCRIPT-ARGV ;

: BF-APPEND-TARGET-REPL-TERM ( ptr u8 n -- ) {: out:ptr outu:n :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/repl-term.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-SNAP-REPL ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-CHECK-OFF
   out outu BF-APPEND-TARGET-REPL-TERM
   out outu s" src/habu/repl.f" BF-APPEND-SOURCE
   out outu s" src/habu/debug-watch.f" BF-APPEND-SOURCE
   out outu s" src/habu/stepper.f" BF-APPEND-SOURCE
   out outu s" src/habu/debug.f" BF-APPEND-SOURCE
   out outu BF-APPEND-FRESH-CHECK-HOOK ;

: BF-APPEND-SNAP-MARK ( ptr u8 n -- )
   s" : SNAP-TAIL-MARK ( -- ) ;" BF-APPEND-LINE ;

: BF-APPEND-SNAP-BUILD ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-APPEND-SNAP-MARK
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-SYS
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-CHECK-OFF
   out outu BF-APPEND-TARGET-IMAGE
   out outu BF-APPEND-FRESH-CHECK-HOOK
   out outu BF-APPEND-IMAGE-TRUSTS
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE
   out outu BF-APPEND-DRIVER-IO ;

: BF-EMIT-SNAP-RUN-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu:n driver:ptr driveru:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-STDIN-RUN-PRELUDE
   out outu BF-APPEND-SNAP-KEEP
   out outu BF-APPEND-SNAP-REPL
   out outu BF-APPEND-SNAP-BUILD
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-CLOSE-CMP ( -- )
   BF-FDA @ dup 0 >= if close else drop then
   BF-FDB @ dup 0 >= if close else drop then
   -1 BF-FDA !
   -1 BF-FDB ! ;

: BF-OPEN-CMP ( ptr u8 n ptr u8 n -- ) {: a:ptr au b:ptr bu :}
   -1 BF-FDA !
   -1 BF-FDB !
   a au FS-PATHZ open-rd BF-FDA !
   BF-FDA @ 0 < if E-BUILD-PATH throw then
   b bu FS-PATHZ open-rd BF-FDB !
   BF-FDB @ 0 < if
      BF-FDA @ close -1 BF-FDA ! E-BUILD-PATH throw
   then ;

: BF-READ-A ( -- n )
   BF-FDA @ BF-CMP-A BF-CMP-CAP read BF-RA !
   BF-RA @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ ;

: BF-READ-B ( -- n )
   BF-FDB @ BF-CMP-B BF-CMP-CAP read BF-RB !
   BF-RB @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ ;

: BF-FILE= ( ptr u8 n ptr u8 n -- bool )
   BF-OPEN-CMP
   begin
      BF-READ-A BF-RA !
      BF-READ-B BF-RB !
      BF-RA @ BF-RB @ <> if BF-CLOSE-CMP BF-FALSE exit then
      BF-RA @ 0= if BF-CLOSE-CMP BF-TRUE exit then
      BF-CMP-A BF-RA @ BF-CMP-B BF-RB @ STR= 0= if
         BF-CLOSE-CMP BF-FALSE exit
      then
   again ;

: BF-TMP-FILE= ( ptr u8 n ptr u8 n -- bool ) {: an:ptr anu bn:ptr bnu :}
   an anu BF-A-PATH BF-TMP> BF-A-LEN !
   bn bnu BF-B-PATH BF-TMP> BF-B-LEN !
   BF-A-PATH BF-A-LEN @ BF-B-PATH BF-B-LEN @ BF-FILE= ;

: BF-STAGE2-SOURCE ( -- )
   s" stage2-src" s" src/habu/stage2.f" BF-EMIT-SOURCE ;

: BF-STDIN-SOURCE ( -- )
   s" stage2-src" s" src/habu/stdin.f" BF-EMIT-STDIN-RUN-SOURCE ;

: BF-SNAP-SOURCE ( -- )
   s" hb-snap-src" s" src/habu/snap.f" BF-EMIT-SNAP-RUN-SOURCE ;

: BF-STAGE2-DIGEST ( ptr u8 -- ) {: dg:ptr :}
   s" stage2-src" BF-A$ dg SHA256-FILE dup 0 <> if throw then drop ;

: BF-RECORD-RESET ( -- )
   0 BF-REC-STAGE? !
   0 BF-REC-STDIN? ! ;

: BF-RECORD-STAGE ( -- )
   BF-REC-STAGE-DG BF-STAGE2-DIGEST
   -1 BF-REC-STAGE? ! ;

: BF-RECORD-STDIN ( -- )
   BF-REC-STDIN-DG BF-STAGE2-DIGEST
   -1 BF-REC-STDIN? ! ;

: BF-BOOTSTRAP-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-REMOVE-TMP
   s" bin/hb" s" stage2-src" BF-A$ BF-RUN-LOAD-STAGE BF-RC0
   s" stage2-got" BF-EXPECT
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-RUN-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-RUN-STAGE-TMP BF-RC0
   s" stage2-got" BF-EXPECT ;

: BF-PROMOTE-STAGE ( -- )
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-VERIFY-STAGE ( -- )
   s" hb-stage" BF-CODESIGN-VERIFY-TMP ;

: BF-STAGE-MATCH? ( -- bool )
   s" hb-stage" s" stage2-got" BF-TMP-FILE= ;

: BF-STAGE-FIXPOINT-FROM-SOURCE ( -- )
   BF-BOOTSTRAP-STAGE
   0 BF-GEN !
   0 BF-FOUND !
   begin BF-GEN @ BF-MAX-GENS < while
      BF-RUN-STAGE
      BF-STAGE-MATCH? if
         BF-VERIFY-STAGE
         -1 BF-FOUND !
         BF-MAX-GENS BF-GEN !
      else
         BF-PROMOTE-STAGE
         BF-GEN @ 1 + BF-GEN !
      then
   repeat
   BF-FOUND @ 0= if s" FIXPOINT BROKEN: no convergence after 4 generations" BF-BUILD-RC die then
   s" bin/hb refresh OK: compiler fixpoint" type cr ;

: BF-STAGE-FIXPOINT ( -- )
   BF-PREFLIGHT
   BF-STAGE2-SOURCE
   BF-RECORD-STAGE
   BF-STAGE-FIXPOINT-FROM-SOURCE ;

: BF-BUILD-STDIN-FROM-STAGE ( -- )
   BF-STDIN-SOURCE
   BF-RECORD-STDIN
   BF-RUN-STAGE
   s" stage2-got" s" hb-stdin-mk" BF-RENAME-TMP
   s" hb-stdin-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-stdin-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-stdin" BF-RENAME-TMP
   s" hb-stdin" BF-CHMOD-X-TMP
   s" hb-stdin" BF-CODESIGN-VERIFY-TMP ;

: BF-BUILD-STDIN ( -- )
   BF-PREFLIGHT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-STDIN-FRESH ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-SNAP-FROM-STDIN ( -- )
   BF-SNAP-SOURCE
   s" hb-snap0" BF-REMOVE-TMP
   s" hb-new" BF-REMOVE-TMP
   s" hb-stdin" s" hb-snap-src" BF-RUN-ENV-TMP-INFILE BF-RC0
   s" hb-snap0" BF-EXPECT
   s" hb-snap0" s" hb-new" BF-RENAME-TMP
   s" hb-new" BF-CODESIGN-FORCE-TMP
   s" hb-new" BF-CHMOD-X-TMP
   s" hb-new" BF-EXPECT
   s" snapshot image OK: candidate validated" type cr ;

: BF-BUILD-ALL ( -- )
   BF-BUILD-STDIN-FRESH ;

: BF-BUILD-SNAP-FRESH ( -- )
   BF-BUILD-ALL
   BF-BUILD-SNAP-FROM-STDIN ;

: BF-INSTALL-HB ( -- )
   s" hb-stdin" BF-A$ s" bin/hb" RENAME-FILE
   s" bin/hb" CHMOD-X ;

: BF-BIN-HB? ( ptr u8 n -- bool )
   s" bin/hb" STR= ;

: BF-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if
      path pathu BF-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: BF-CLEAN-BIN ( -- )
   s" bin" [: BF-REMOVE-BIN-OTHER ;] WALK-FILES ;

: BF-INSTALL ( -- )
   BF-BUILD-STDIN-FRESH
   BF-INSTALL-HB
   BF-CLEAN-BIN
   s" bin/hb ready (small checked engine, tty REPL + stdin)" type cr ;

: BF-ENGINE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-ENGINE-BUF u BYTE-COPY
   u BF-ENGINE-U ! ;

: BF-ENGINE-RESET ( -- )
   0 BF-ENGINE-U ! ;

: BF-ENGINE$ ( -- ptr u8 n )
   BF-ENGINE-U @ 0 > if BF-ENGINE-BUF BF-ENGINE-U @ exit then
   s" HABU_FIXPOINT_ENGINE" GETENV dup 0 > if exit then drop drop
   s" bin/hb" ;

: BF-STAMP-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-BUILD-PATH throw then
   u FS-PATH-CAP > if E-BUILD-PATH throw then
   a BF-STAMP-PATH-BUF u BYTE-COPY
   u BF-STAMP-PATH-U ! ;

: BF-STAMP-PATH-RESET ( -- )
   0 BF-STAMP-PATH-U ! ;

: BF-STAMP-DIR-XDG? ( -- bool )
   s" XDG_CACHE_HOME" GETENV dup 0= if drop drop BF-FALSE exit then
   s" habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U !
   BF-TRUE ;

: BF-STAMP-DIR-HOME? ( -- bool )
   s" HOME" GETENV dup 0= if drop drop BF-FALSE exit then
   s" .cache/habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U !
   BF-TRUE ;

: BF-STAMP-DIR-TMP ( -- )
   s" TMPDIR" GETENV dup 0= if drop drop s" /tmp" then
   s" habu-fixpoint" BF-STAMP-DIR-BUF JOIN-PATH BF-STAMP-DIR-U ! ;

: BF-STAMP-DIR$ ( -- ptr u8 n )
   BF-STAMP-DIR-XDG? if BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ exit then
   BF-STAMP-DIR-HOME? if BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ exit then
   BF-STAMP-DIR-TMP
   BF-STAMP-DIR-BUF BF-STAMP-DIR-U @ ;

: BF-STAMP-DEFAULT? ( -- bool )
   BF-STAMP-PATH-U @ 0 > if BF-FALSE exit then
   s" HABU_FIXPOINT_STAMP" GETENV nip 0= ;

: BF-STAMP-PATH$ ( -- ptr u8 n )
   BF-STAMP-PATH-U @ 0 > if BF-STAMP-PATH-BUF BF-STAMP-PATH-U @ exit then
   s" HABU_FIXPOINT_STAMP" GETENV dup 0 > if exit then drop drop
   BF-STAMP-DIR$ s" stamp" BF-STAMP-DEF-BUF JOIN-PATH BF-STAMP-DEF-U !
   BF-STAMP-DEF-BUF BF-STAMP-DEF-U @ ;

: BF-PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1 -
      a over + c@ BF-SLASH = if exit then
   repeat ;

: BF-STAMP-ENSURE-DIR ( -- )
   BF-STAMP-DEFAULT? if BF-STAMP-DIR$ MAKE-DIRS exit then
   BF-STAMP-PATH$ {: a:ptr u:n :}
   a u BF-PARENT-U {: pu:n :}
   pu 0 > if a pu MAKE-DIRS then ;

: BF-STAMP-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BF-STAMP-U @ u + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   a BF-STAMP-BUF BF-STAMP-U @ + u BYTE-COPY
   BF-STAMP-U @ u + BF-STAMP-U ! ;

: BF-STAMP-C+ ( n -- ) {: c:n :}
   BF-STAMP-U @ 1 + BF-STAMP-CAP > if E-STR-CAPACITY throw then
   c BF-STAMP-BUF BF-STAMP-U @ + c!
   BF-STAMP-U @ 1 + BF-STAMP-U ! ;

: BF-STAMP-FRAG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u BF-STAMP-C+
   a u BF-STAMP-BYTES+ ;

: BF-STAMP-DG+ ( ptr u8 n ptr u8 -- ) {: tag:ptr tagu:n dg:ptr :}
   tag tagu BF-STAMP-FRAG+
   dg BF-STAMP-DG-U BF-STAMP-FRAG+ ;

: BF-STAMP-ENGINE+ ( -- )
   BF-ENGINE$ BF-STAMP-DG SHA256-FILE dup 0 <> if throw then drop
   s" engine" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-KEY-BEGIN ( -- )
   0 BF-STAMP-U !
   s" build-fixpoint-stamp-v1" BF-STAMP-FRAG+
   BF-STAMP-ENGINE+ ;

: BF-STAMP-STAGE-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stage2-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-STDIN-KEY+ ( -- )
   BF-STAMP-DG BF-STAGE2-DIGEST
   s" stdin-src" BF-STAMP-DG BF-STAMP-DG+ ;

: BF-STAMP-KEY-END ( -- )
   BF-STAMP-BUF BF-STAMP-U @ BF-STAMP-DG SHA256
   BF-STAMP-DG BF-STAMP-KEY SHA256>HEX ;

: BF-STAMP-KEY! ( -- )
   BF-STAMP-KEY-BEGIN
   BF-STAGE2-SOURCE
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BF-STAMP-RECORDED-KEY! ( -- )
   BF-REC-STAGE? @ 0= if E-BUILD-STATUS throw then
   BF-REC-STDIN? @ 0= if E-BUILD-STATUS throw then
   BF-STAMP-KEY-BEGIN
   s" stage2-src" BF-REC-STAGE-DG BF-STAMP-DG+
   s" stdin-src" BF-REC-STDIN-DG BF-STAMP-DG+
   BF-STAMP-KEY-END ;

: BF-STAMP-READ? ( -- bool )
   BF-STAMP-PATH$ FILE? 0= if BF-FALSE exit then
   BF-STAMP-PATH$ FILE-SIZE BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-PATH$ BF-STAMP-OLD BF-STAMP-HEX-U 1 + READ-ALL BF-STAMP-HEX-U 1 + <> if BF-FALSE exit then
   BF-STAMP-OLD BF-STAMP-HEX-U + c@ BF-LF <> if BF-FALSE exit then
   BF-TRUE ;

: BF-STAMP-MATCH? ( -- bool )
   BF-FORCE @ 0 <> if BF-FALSE exit then
   BF-STAMP-READ? 0= if BF-FALSE exit then
   BF-STAMP-KEY!
   BF-STAMP-OLD BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= ;

: BF-STAMP-WRITE ( -- )
   BF-STAMP-ENSURE-DIR
   BF-STAMP-RECORDED-KEY!
   BF-LF BF-STAMP-KEY BF-STAMP-HEX-U + c!
   BF-STAMP-PATH$ BF-STAMP-KEY BF-STAMP-HEX-U 1 + ATOMIC-WRITE-FILE ;

: BF-STAMP-CACHED ( -- )
   s" fixpoint: cached " type
   BF-STAMP-KEY BF-STAMP-PREFIX-U type cr ;

: BF-STDIN-HB= ( -- bool )
   s" hb-stdin" BF-A$ BF-ENGINE$ BF-FILE= ;

: BF-ALL-STAMP ( -- )
   BF-STDIN-HB= if BF-STAMP-WRITE then ;

: BF-BUILD-ALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-BUILD-ALL
   BF-ALL-STAMP ;

: BF-INSTALL-CACHED ( -- )
   BF-STAMP-MATCH? if BF-STAMP-CACHED exit then
   BF-RECORD-RESET
   BF-INSTALL
   BF-STAMP-WRITE ;

: BF-USAGE ( -- )
   s" usage: tools/build-fixpoint.f [all|install|stage|stdin|snap] [--force]" BF-USAGE-RC die ;

: BF-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: BF-ARGN= ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx SCRIPT-ARGV$ a u STR= ;

: BF-PARSE-FORCE ( -- n )
   0 BF-FORCE !
   SCRIPT-ARGC 0= if 0 exit then
   SCRIPT-ARGC 1 - s" --force" BF-ARGN= if
      -1 BF-FORCE !
      SCRIPT-ARGC 1 - exit
   then
   SCRIPT-ARGC ;

: BF-MAIN ( -- )
   BF-PARSE-FORCE {: argn:n :}
   argn 0= if BF-BUILD-ALL-CACHED exit then
   argn 1 <> if BF-USAGE then
   s" all" BF-ARG0= if BF-BUILD-ALL-CACHED exit then
   s" install" BF-ARG0= if BF-INSTALL-CACHED exit then
   s" stage" BF-ARG0= if BF-STAGE-FIXPOINT exit then
   s" stdin" BF-ARG0= if BF-BUILD-STDIN-FRESH exit then
   s" snap" BF-ARG0= if BF-BUILD-SNAP-FRESH exit then
   BF-USAGE ;
