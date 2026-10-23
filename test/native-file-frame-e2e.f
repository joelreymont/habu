\ A file frame records each dependency the file consumes from earlier files
\ once, and none of the records the file makes itself. Each file that compiles
\ whole also writes one object under a key that names the file's place in the
\ load, not its text, so an edit keeps the key. Its definitions' result
\ digests change with their code and not with a comment or a relocation.
\ bin/hb --load test/native-file-frame-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-FRAME-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U
create WANT CAP allot variable WANT-U
64 constant KEY-LEN
create APP-KEY KEY-LEN allot
create LIB-KEY KEY-LEN allot
create RU-KEY KEY-LEN allot
16 constant RES-LEN
create APP-RES RES-LEN allot
create LIB-RES RES-LEN allot
24 constant AT-CAP
create HITS-AT AT-CAP allot variable HITS-AT-U
DYNAMIC-BUFFER OBJ u8
DYNAMIC-BUFFER WARM u8
16 constant OBJ-HEAD
512 constant LINE-CAP
create APP-LINE LINE-CAP allot variable APP-LINE-U
create DEP-LINE LINE-CAP allot variable DEP-LINE-U
create OG-LINE LINE-CAP allot variable OG-LINE-U
create OG-MOVED LINE-CAP allot variable OG-MOVED-U
\ Nonzero from the first build after the warm one on: og.f loads under the
\ fixture tree's engine root instead of the repository's, jrn.f under
\ LOWER-CERT-HOOK's check hook with the early origin journal still armed, and
\ pcf.f, pcz.f, pcd.f and pce.f where a journal records outside every sink.
variable MOVED

: IN+ ( ptr u8 n -- ) INPUT CAP INPUT-U BUF-APPEND ;

: LOAD+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   S\" s\" " IN+ name nameu AT IN+ S\" \" s\" repl\" NDEF-CACHE:LOAD\n" IN+ ;

\ Load name between two reads of the early journal's serial, and print tag
\ and how many rows the load recorded. A full journal row takes its serial
\ from the early journal too.
: EK-LOAD+ ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n tag:ptr tagu:n :}
   S\" FRAME-EK-PROBE:EK-SER\n" IN+
   name nameu LOAD+
   S\" FRAME-EK-PROBE:EK-SER swap - s\" " IN+ tag tagu IN+ S\" \" type . cr\n" IN+ ;

\ app.f's text, which its object's source spans are checked against.
: APP$ ( -- ptr u8 n )
   S\" require base.f\nrequire run.f\nrequire safe.f\nrequire vread.f\n: PRE ( -- n ) FRAME-BASE:ONE ;\nrequire lib.f\n: IMM-USER ( -- n ) FRAME-LIB:MARK 5 ;\n: FRAME-USE ( -- n )\n   FRAME-LIB:SEVEN FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-BASE:ONE + FRAME-DEEP:TEN + ;\nrequire exec.f\n: TALLY ( -- ) 1 FRAME-LIB:HITS +! ;\nTALLY\n' FRAME-LIB:QUAD FRAME-LIB:HOOK xt!\n" ;

\ A builder session's first lines: the tier its loads compile at and the
\ capture's preparation. LOAD+ lines follow, then RUN-INPUT.
: SESSION ( -- )
   0 INPUT-U !
   S\" 1 set-tier\nNATIVE-RUNTIME:CAPTURE-PREPARE\n" IN+ ;

\ Run the session INPUT holds on the builder over the build cache, with the
\ cache diagnostics on; its exit code.
: RUN-INPUT ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

\ Set the builder's engine root to root.
: ROOT+ ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   S\" s\" " IN+ root rootu IN+ S\" \" SOURCE-ROOT:ENGINE!\n" IN+ ;

\ The diagnostic builder session: each file loads through NDEF-CACHE:LOAD with
\ the cache diagnostics on, so every file frame and object prints its line on
\ stderr. diag.f binds DIAGXT, a deferred word whose link the checker handover
\ carries to the engine checker's own kept row (src/core/checker.f
\ TRANSFER-ROW). Then a multi-error run refuses BROKEN's body at tier 0 and
\ keeps its declared row as a recovery fact; broken.f binds BROKEN under the
\ same run. BROKEN is made by broken.f's child brokendef.f, since a record
\ made between top-level loads has no key a file can name. after.f, the first file after the run ends, stores its definition:
\ the run's last recovery binding leaves the checker idle.
: PROBE ( -- n )
   SESSION
   \ dv.f loads first: check.f, which top.f requires, allots its object's
   \ path, so what follows it stands elsewhere in a cell on each build. The
   \ moved builds allot a byte before it, which leaves its DATA padded for
   \ another place in a cell.
   MOVED @ if S\" 1 allot\n" IN+ then
   s" dv.f" LOAD+
   S\" s\" dv=\" type DV-SUM . cr\ns\" dvs=\" type DV-S type cr\n" IN+
   s" df.f" LOAD+
   s" top.f" LOAD+
   S\" s\" use=\" type FRAME-USE . cr\n" IN+
   S\" s\" hits=\" type FRAME-LIB:HITS NULL-PTR - . space cr\n" IN+
   s" con-dep.f" LOAD+
   S\" s\" con=\" type CON-USE . cr\n" IN+
   S\" s\" dep=\" type CON-DEP . cr\n" IN+
   s" rp.f" LOAD+
   S\" s\" rp=\" type RP-B . cr\n" IN+
   s" hpar.f" LOAD+
   S\" s\" hp=\" type FRAME-HP:B . HG-ONE . HG-TWO . cr\n" IN+
   s" w.f" LOAD+
   S\" s\" w=\" type wordlist W - . s\" k=\" type K . cr\n" IN+
   S\" 0 set-tier\n" IN+
   s" tier0.f" LOAD+
   S\" 1 set-tier\ns\" tier=\" type TIER-K . cr\n" IN+
   s" td.f" LOAD+
   S\" s\" td=\" type TD-K . cr\n" IN+
   s" pkroot.f" LOAD+
   S\" s\" pk=\" type FRAME-PK:PK-VAL . cr\n" IN+
   \ og.f and ogw.f load under the full origin journal, which the capture
   \ preparation above disarmed. The row it holds for each of their words:
   \ its name, whether it exists and its name token span. Their tree lies
   \ outside the engine root, so no row has a path. OJ-SHOW prints the full
   \ journal's rows past a serial in their order: name and name token span.
   MOVED @ if ROOT$ else SOURCE-ROOT:CWD$ then ROOT+
   S\" package FRAME-OJ-PROBE\nCAST: OJ-MARK ( n -- [ -- n n bool ] )\nCAST: OJ-ROWS ( n -- [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] )\n: OJ-ROW ( n n n n ptr u8 n ptr u8 n n n -- )\n   {: serial:n phase:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n at:n end:n :}\n   space name nameu type s\" :\" type at . end . ;\npublic\n: OJ-SER ( -- n ) s\" ENGINE-ORIGIN-EARLY:FILE-MARK\" NUNIT-ORIGIN:JOURNAL-XT OJ-MARK execute drop drop ;\n: OJ-SHOW ( n -- ) s\" oj=\" type [: OJ-ROW ;] s\" ENGINE-ORIGIN:FILE-ROWS\" NUNIT-ORIGIN:JOURNAL-XT OJ-ROWS execute s\" ;oj\" type cr ;\n;package\n" IN+
   S\" ENGINE-ORIGIN:ARM\n" IN+
   s" og.f" LOAD+
   s" ogw.f" LOAD+
   S\" FRAME-OJ-PROBE:OJ-SER\n" IN+
   s" hop.f" LOAD+
   S\" FRAME-OJ-PROBE:OJ-SHOW\n" IN+
   S\" ENGINE-ORIGIN:DISARM\n" IN+
   SOURCE-ROOT:CWD$ ROOT+
   S\" package FRAME-OG-PROBE\nCAST: OG-SYM-XT ( n -- [ ptr u8 n n ptr u8 n -- n ] )\n: OG-ROW ( ptr u8 n n ptr u8 n -- )\n   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:ORIGIN-SYM-OFF + CELL-VIEW @ OG-SYM-XT execute\n   ENGINE-ORIGIN:TARGET drop {: at:n end:n name:ptr nameu:n :} 2drop\n   space name nameu type s\" :\" type nameu 0<> if 1 else 0 then . at . end . ;\ns\" origins=\" type s\" FRAME-OG\" 2 s\" OG-P\" OG-ROW s\" \" 0 s\" OG-G\" OG-ROW s\" \" 0 s\" OG-W\" OG-ROW s\" ;origins\" type cr\n;package\n" IN+
   s" watermark.f" LOAD+
   s" ret-own.f" LOAD+
   s" ret-parent.f" LOAD+
   s" ret-late.f" LOAD+
   s" incut.f" LOAD+
   s" gp.f" LOAD+
   s" q1.f" LOAD+
   s" diag.f" LOAD+
   S\" MULTI-ERR-BEGIN\n" IN+
   s" broken.f" LOAD+
   s" multi.f" LOAD+
   S\" s\" refused=\" type MULTI-ERR-END .\n" IN+
   s" after.f" LOAD+
   s" aftpar.f" LOAD+
   s" retire.f" LOAD+
   s" raw.f" LOAD+
   s" poke.f" LOAD+
   s" slot.f" LOAD+
   s" later.f" LOAD+
   s" list.f" LOAD+
   s" addr.f" LOAD+
   s" upd.f" LOAD+
   s" off.f" LOAD+
   s" own.f" LOAD+
   s" ev.f" LOAD+
   s" evw.f" LOAD+
   s" evx.f" LOAD+
   s" evr.f" LOAD+
   s" ev2.f" LOAD+
   s" tierpar.f" LOAD+
   S\" 1 set-tier\n" IN+
   s" tierback.f" LOAD+
   s" pkgload.f" LOAD+
   s" newtype.f" LOAD+
   s" ffl.f" LOAD+
   S\" s\" crc=\" type 0 NULL-PTR 0 FF-CRC . cr\n" IN+
   s" ffl2.f" LOAD+
   S\" s\" crc2=\" type 0 NULL-PTR 0 FF-CRC2 . cr\n" IN+
   s" ffn.f" LOAD+
   S\" s\" pid=\" type FF-PID 0 > . cr\n" IN+
   s" sel.f" LOAD+
   s" selc.f" LOAD+
   s" ffb.f" LOAD+
   s" sela.f" LOAD+
   s" selb.f" LOAD+
   s" scpar.f" LOAD+
   s" sesa.f" LOAD+
   s" plain.f" LOAD+
   s" wra.f" LOAD+
   s" wrb.f" LOAD+
   S\" s\" wrb=\" type FFI-DECL:SELECTION . . . cr\n" IN+
   s" wca.f" LOAD+
   s" wcb.f" LOAD+
   S\" s\" wcb=\" type FFI-DECL:SELECTION . . . cr\n" IN+
   S\" package FRAME-P3\n" IN+
   s" pkgc.f" LOAD+
   s" cread.f" LOAD+
   s" act-ok.f" LOAD+
   s" fw-def.f" LOAD+
   s" out.f" LOAD+
   s" reach.f" LOAD+
   s" mask.f" LOAD+
   s" own-trust.f" LOAD+
   s" parse-ok.f" LOAD+
   s" tier-ok.f" LOAD+
   s" fwrite.f" LOAD+
   s" fenv.f" LOAD+
   s" dbase.f" LOAD+
   s" nestev.f" LOAD+
   s" nestdef.f" LOAD+
   s" nest2.f" LOAD+
   s" nestafter.f" LOAD+
   s" nestlit.f" LOAD+
   s" nestthrow.f" LOAD+
   s" exec-xt.f" LOAD+
   s" act2.f" LOAD+
   s" nest-parent.f" LOAD+
   s" act.f" LOAD+
   S\" s\" session=\" type NCOMP:SESSION-LIVE? . cr\n" IN+
   \ jrn.f loads before scope.f, the last load, under the early origin
   \ journal, armed, with its own check hook or, once MOVED,
   \ LOWER-CERT-HOOK's; ek= counts the rows its load recorded. The hooks stay
   \ installed for scope.f. pcf.f, pcz.f, pcd.f and pce.f first load where
   \ no journal records; once MOVED, where one records outside every sink.
   \ Both exports load with checking off, as both keys require: another hook
   \ refuses the file by itself. pcf.f's export goes to the full journal,
   \ armed; pce.f's to the early journal, armed and installed. pcz.f's
   \ constant loads in pcf.f's state and records nothing, so only the open
   \ refuses it. pcd.f's constant loads under the early journal's hook with
   \ that journal disarmed. pcf= and the others count the rows each load
   \ recorded.
   S\" package FRAME-EK-PROBE\nprivate\nCAST: EK-ACT ( n -- [ -- ] )\nCAST: EK-MARK ( n -- [ -- n n bool ] )\nCAST: EK-REST ( n -- [ n bool -- ] )\npublic\n: EK-SER ( -- n ) s\" ENGINE-ORIGIN-EARLY:FILE-MARK\" NUNIT-ORIGIN:JOURNAL-XT EK-MARK execute drop drop ;\n: EK-SET ( bool -- ) {: on:bool :} s\" ENGINE-ORIGIN-EARLY:FILE-MARK\" NUNIT-ORIGIN:JOURNAL-XT EK-MARK execute drop nip on s\" ENGINE-ORIGIN-EARLY:FILE-RESTORE\" NUNIT-ORIGIN:JOURNAL-XT EK-REST execute ;\n: EK-DO ( ptr u8 n -- ) NUNIT-ORIGIN:JOURNAL-XT EK-ACT execute ;\n;package\n" IN+
   MOVED @ if
      S\" 0 set-check ENGINE-ORIGIN:ARM\n" IN+
      s" pcf.f" s" pcf=" EK-LOAD+
      s" pcz.f" s" pcz=" EK-LOAD+
      S\" ENGINE-ORIGIN:DISARM\ns\" ENGINE-ORIGIN-EARLY:INSTALL\" FRAME-EK-PROBE:EK-DO 0 set-check true FRAME-EK-PROBE:EK-SET\n" IN+
      s" pce.f" s" pce=" EK-LOAD+
      S\" s\" ENGINE-ORIGIN-EARLY:INSTALL\" FRAME-EK-PROBE:EK-DO false FRAME-EK-PROBE:EK-SET\n" IN+
   else
      S\" 0 set-check\n" IN+
      s" pcf.f" s" pcf=" EK-LOAD+
      s" pcz.f" s" pcz=" EK-LOAD+
      s" pce.f" s" pce=" EK-LOAD+
      S\" s\" ENGINE-ORIGIN:INSTALL\" FRAME-EK-PROBE:EK-DO\n" IN+
   then
   s" pcd.f" s" pcd=" EK-LOAD+
   S\" s\" ENGINE-ORIGIN-EARLY:INSTALL\" FRAME-EK-PROBE:EK-DO\n" IN+
   MOVED @ if S\" s\" LOWER-CERT-HOOK:INSTALL\" FRAME-EK-PROBE:EK-DO\n" IN+ then
   S\" true FRAME-EK-PROBE:EK-SET\n" IN+
   s" jrn.f" s" ek=" EK-LOAD+
   s" scope.f" LOAD+
   RUN-INPUT ;

\ The stderr line whose path ends in name followed by tag, from that tail to
\ its newline: tag " rows=" finds the file frame, " key=" the file object.
\ The match starts at the path's last "/", so own.f never finds ret-own.f.
: LINE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n tag:ptr tagu:n :}
   0 INPUT-U !
   s" /" IN+ name nameu IN+ tag tagu IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N 1+ {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: FRAME$ ( ptr u8 n -- ptr u8 n ) s"  rows=" LINE$ ;
: OBJECT$ ( ptr u8 n -- ptr u8 n ) s"  key=" LINE$ ;

\ The value of field (" name=") in line, up to the next space or the end.
: FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: line:ptr lineu:n field:ptr fieldu:n :}
   line lineu field fieldu FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N fieldu + {: at:n :}
        line at +  lineu at -  {: a:ptr u:n :}
        a u 32 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

\ A field's decimal value.
: FIELD-N ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF T-FAIL 0 ENDOF
     some OF ENDOF
   ;MATCH ;

: KEY$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  key=" FIELD$ ;
: DEFS$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  defs=" FIELD$ ;
: RESULTS$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  results=" FIELD$ ;
: RESOLUTIONS$ ( ptr u8 n -- ptr u8 n ) FRAME$ s"  resolution=" FIELD$ ;

\ name's object line holds field with the value expected.
: OBJECT-HAS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n field:ptr fieldu:n want:ptr wantu:n :}
   name nameu OBJECT$ field fieldu FIELD$ want wantu T$= ;

\ name's object holds count input rows.
: INPUTS ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n count:ptr countu:n :}
   name nameu OBJECT$ s"  inputs=" FIELD$ count countu T$= ;

\ The object file the key names exists in the build cache.
: STORED ( ptr u8 n -- )
   {: key:ptr keyu:n :}
   0 INPUT-U !
   s" cache/hb-file-" IN+ key keyu IN+ s" .hfo" IN+
   INPUT INPUT-U @ AT EXISTS? TTRUE ;

: KEEP ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dest:ptr destu:n :}
   srcu destu T=
   src dest srcu destu min BYTE-COPY ;

: LIB-SOURCE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: head:ptr headu:n twice:ptr twiceu:n mark:ptr marku:n :}
   0 INPUT-U !
   head headu IN+
   S\" require base.f\nrequire deep.f\npackage FRAME-LIB\npublic\n: TWICE ( n -- n ) " IN+
   twice twiceu IN+
   S\"  ;\n: MARK ( -- ) " IN+
   mark marku IN+
   S\"  ; immediate\nvariable HITS\ncreate T 16 allot\nTYPED-VARIABLE HOOK [ n -- n ]\n: QUAD ( n -- n ) TWICE TWICE FRAME-BASE:ONE + ;\nTRUSTED: BUMP ( n -- n ) 1 + ;\n7 constant SEVEN\n' TWICE HOOK xt!\n;package\ns\" FRAME-LIB:MARK\" 0 parse-imm\n" IN+
   s" lib.f" INPUT INPUT-U @ PUT ;

\ check.f, which top.f requires after app.f, checks the first build's app.f
\ dependency rows, kept in app-cold.obj, while the session that loaded app.f
\ is still open: the rows' record keys exist only there. It first undoes
\ app.f's two writes to lib.f's DATA, TALLY's to HITS and the store of QUAD
\ into HOOK, so lib.f's DATA stands as app.f found it, as it does where a
\ build checks the rows, before app.f. For each split k it prints one digit,
\ 1 when rows [0,k) still hold plus 2 when rows [k,n) do, then for each row
\ i a 1 when row i holds, else 0.
\ Section 14 is the object's dependency rows (NUNIT-OBJECT's DEPS-SLOT).
: CHECKER ( -- )
   0 INPUT-U !
   S\" -1 FRAME-LIB:HITS +!\n' FRAME-LIB:TWICE FRAME-LIB:HOOK xt!\n" IN+
   S\" : SPLITS ( ptr u8 n -- ) {: deps:ptr depsu:n :}\n   deps depsu NUNIT-DEP:FORMAT {: rows:n :}\n   s\" split=\" type\n   rows 1+ 0 ?do\n      deps depsu 0 i NUNIT-DEP:CHECK-ROWS if 1 else 0 then\n      deps depsu i rows NUNIT-DEP:CHECK-ROWS if 2 else 0 then + 48 + emit\n   loop cr\n   s\" held=\" type\n   rows 0 ?do\n      deps depsu i i 1+ NUNIT-DEP:CHECK-ROWS if 49 else 48 then emit\n   loop cr ;\ns\" " IN+
   s" app-cold.obj" AT IN+
   S\" \" NUNIT-FILE:ARCH-AARCH64 s\" FILE\" s\" " IN+
   APP-KEY KEY-LEN IN+
   S\" \" NUNIT-FILE:READ\n14 NUNIT-FILE:SECTION$ SPLITS\n" IN+
   s" check.f" INPUT INPUT-U @ PUT ;

\ Keep the object stored under key, past its cache entry head, as file name.
: KEEP-OBJ ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n name:ptr nameu:n :}
   0 INPUT-U !
   s" cache/hb-file-" IN+ key keyu IN+ s" .hfo" IN+
   INPUT INPUT-U @ AT {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size OBJ-HEAD > dup TTRUE 0= if exit then
   size OBJ-RESERVE
   path pathu 0 OBJ BYTE-VIEW size READ-ALL size <> if E-FS-IO throw then
   name nameu 0 OBJ BYTE-VIEW OBJ-HEAD BYTE+ size OBJ-HEAD - PUT ;

: KEEP-OBJECT ( -- ) s" app.f" KEY$ s" app-cold.obj" KEEP-OBJ ;

\ The digits the session printed after tag, and how many of them are c.
: DIGITS$ ( ptr u8 n -- ptr u8 n )
   {: tag:ptr tagu:n :}
   OUT$ tag tagu FIELD$ {: a:ptr u:n :}
   a u 10 INDEX-OF MATCH option
     none OF a u ENDOF
     some OF IDX>N a swap ENDOF
   ;MATCH ;
: DIGIT-COUNT ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}
   0 u 0 ?do a i + c@ c = if 1+ then loop ;
: SPLIT$ ( -- ptr u8 n ) s" split=" DIGITS$ ;
: SPLIT-COUNT ( n -- n ) {: c:n :} SPLIT$ c DIGIT-COUNT ;
: HELD-COUNT ( n -- n ) {: c:n :} s" held=" DIGITS$ c DIGIT-COUNT ;

\ A rebuild after an edit to lib.f: the app still runs, both files still
\ write objects with every colon definition, under the keys of the first build.
: REBUILT ( ptr u8 n -- )
   T-LABEL
   PROBE SUCCESS
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   s" app.f" DEFS$ s" 4" T$=
   s" lib.f" DEFS$ s" 5" T$=
   s" app.f" KEY$ APP-KEY KEY-LEN T$=
   s" lib.f" KEY$ LIB-KEY KEY-LEN T$=
   s" app.f" RESULTS$ APP-RES RES-LEN T$= ;

\ name's frame is unordered and holds one run and one data read.
: RAN ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   name nameu FRAME$ s"  executed=" FIELD$ s" 1" T$=
   name nameu FRAME$ s"  data=" FIELD$ s" 1" T$= ;

\ HITS's address as the probe printed it, up to the space after it.
: HITS$ ( -- ptr u8 n ) OUT$ s" hits=" FIELD$ ;

20 constant DIGITS-CAP
create DIGITS DIGITS-CAP allot variable DIGITS-AT

\ n in decimal, appended to INPUT.
: N+ ( n -- )
   DIGITS-CAP DIGITS-AT !
   begin
      -1 DIGITS-AT +!
      dup 10 mod 48 + DIGITS DIGITS-AT @ + c!
      10 / dup 0=
   until drop
   DIGITS DIGITS-AT @ + DIGITS-CAP DIGITS-AT @ - IN+ ;

\ The span of definition name's name token in app.f, as "at-end", appended
\ to INPUT: what the origin journal records for it.
: SPAN+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   INPUT-U @ {: mark:n :}
   s" : " IN+ name nameu IN+ s"  " IN+
   APP$ INPUT mark + INPUT-U @ mark - FIND-SUB MATCH option
     none OF T-FAIL 0 ENDOF
     some OF IDX>N 2 + ENDOF
   ;MATCH {: at:n :}
   mark INPUT-U !
   at N+ s" -" IN+ at nameu + N+ ;

\ The source spans app.f's object holds, one per definition in order.
: APP-SPANS ( -- )
   0 INPUT-U !
   s" PRE" SPAN+ s" ," IN+
   s" IMM-USER" SPAN+ s" ," IN+
   s" FRAME-USE" SPAN+ s" ," IN+
   s" TALLY" SPAN+
   INPUT WANT INPUT-U @ BYTE-COPY INPUT-U @ WANT-U !
   s" app.f" OBJECT$ s"  src=" FIELD$ WANT WANT-U @ T$= ;

\ name's load wrote no object. The match starts at the path's last "/", so
\ run.f never finds aftrun.f.
: NO-OBJECT ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   0 INPUT-U !
   s" /" IN+ name nameu IN+ s"  key=" IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF ENDOF
     some OF drop T-FAIL ENDOF
   ;MATCH ;

\ name's frame ends in the run reason given, and its load wrote no object.
: RUN-REFUSED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n why:ptr whyu:n :}
   name nameu FRAME$ why whyu ENDS-WITH? TTRUE
   name nameu NO-OBJECT ;

\ Files whose installs nest under an installing parent, kept apart from
\ FIXTURES for its body text limit (BODYBUF-CAP).
: NEST-FIXTURES ( -- )
   \ hpar.f requires hch.f between its two segments, and hch.f requires hg1.f
   \ and hg2.f: once a comment makes hch.f compile, hg1.f's and hg2.f's
   \ installs run inside it while hpar.f's install holds its segment map.
   s" hg1.f" S\" : HG-ONE ( -- n ) 1 ;\n" PUT
   s" hg2.f" S\" : HG-TWO ( -- n ) 2 ;\n" PUT
   s" hch.f" S\" require hg1.f\nrequire hg2.f\n" PUT
   s" hpar.f" S\" package FRAME-HP public : A ( -- n ) 1 ; ;package\nrequire hch.f\npackage FRAME-HP public : B ( -- n ) A ; ;package\n" PUT
   \ hop.f loads hoc.f between its definitions under the full origin journal.
   s" hoc.f" S\" : HO-PC ( -- n ) 3 ;\n" PUT
   s" hop.f" S\" : HO-PA ( -- n ) 1 ;\nrequire hoc.f\n: HO-PB ( -- n ) 2 ;\n" PUT ;

\ The FFI fixtures, kept apart from FIXTURES: together their body text
\ passes the 8000 bytes one definition captures (BODYBUF-CAP).
: FFI-FIXTURES ( -- )
   \ A file that grows a registry table refuses. ffl.f, the first FFI file,
   \ declares functions until the table grows, from any count: the rest fit.
   \ ffl2.f reuses its library row. ffn.f runs PROCESS-SYMBOLS before its
   \ FUNCTION: appends a row.
   s" ffl.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: FF-CRC crc32 ( n ptr u8 n -- n ) ;FUNCTION\n4 BUFFER: GROW-SYM\n: GROW-NAME ( n -- ) 102 GROW-SYM c! 4 1 do dup 26 mod 97 + GROW-SYM i + c! 26 / loop drop ;\n: GROW ( -- ) FFI:MARK {: room:n :} 0 begin dup GROW-NAME GROW-SYM 4 FFI:PROCESS 0 FFI:DECLARE drop 1+ FFI:MARK room <> until drop ;\nGROW\n" PUT
   s" ffn.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: FF-PID getpid ( -- i32 ) ;FUNCTION\n" PUT
   s" ffl2.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: FF-CRC2 crc32 ( n ptr u8 n -- n ) ;FUNCTION\n" PUT
   \ sel.f declares getuid under the process selection; selc.f, which does not
   \ require sel.f, declares the same row: a reuse row and the entry selection.
   s" sel.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: SEL-UID getuid ( -- i32 ) ;FUNCTION\n" PUT
   s" selc.f" S\" require src/lib/ffi-abi.f\nFUNCTION: SELC-UID getuid ( -- i32 ) ;FUNCTION\n" PUT
   \ ffb.f declares into the maker bank, which its segment's open mark did not
   \ select.
   s" ffb.f" S\" require src/lib/ffi-abi.f\n: FFB ( -- ) s\" getppid\" FFI:PROCESS 0 FFI:DECLARE drop ;\n' FFB FFI:WITH-MAKER-DECLARATIONS\n1 constant FFB-K\n" PUT
   \ sela.f selects what was selected; selb.f selects it again and declares
   \ nothing. Each top-level PROCESS-SYMBOLS refuses its file.
   s" sela.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" PUT
   s" selb.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n17 constant SELB-K\n" PUT
   \ scopea.f leaves its package's wordlist selected; scopeb.f, which does
   \ not require scopea.f, opens with that selection. A package an earlier
   \ session made has no key, so scpar.f loads both in its own.
   s" scpar.f" S\" require scopea.f\nrequire scopeb.f\n" PUT
   s" scopea.f" S\" require src/lib/ffi-abi.f\npackage FRAME-SCA\nPROCESS-SYMBOLS\n;package\n" PUT
   s" scopeb.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: SCB-PID getpid ( -- i32 ) ;FUNCTION\n" PUT
   \ sesa.f, a session of its own, leaves its package's wordlist selected;
   \ the next session has no key for it. plain.f, in that session, neither
   \ reads nor changes the selection, so it stores all the same.
   s" sesa.f" S\" require src/lib/ffi-abi.f\npackage FRAME-SESA\nPROCESS-SYMBOLS\n;package\n" PUT
   s" plain.f" S\" 17 constant PLAIN-K\n" PUT
   \ wrb.f's only FFI action, an FFI-DECL:SELECT, writes the selection wra.f
   \ left as it found it.
   s" wra.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" PUT
   s" wrb.f" S\" require src/lib/ffi-abi.f\n0 0 true FFI-DECL:SELECT\n17 constant WRB-K\n" PUT
   \ wcb.f's IMAGE-LIFECYCLE:PREPARE runs FFI-DECL's CLEAR-SCOPE hook, which
   \ clears the scope and keeps the library wca.f selected.
   s" wca.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\n" PUT
   s" wcb.f" S\" require src/lib/image-lifecycle.f\nIMAGE-LIFECYCLE:PREPARE\n17 constant WCB-K\n" PUT ;

: FIXTURES ( -- )
   s" base.f" S\" package FRAME-BASE\npublic\n: ONE ( -- n ) 1 ;\n;package\n" PUT
   s" deep.f" S\" package FRAME-DEEP\npublic\n: TEN ( -- n ) 10 ;\n;package\n" PUT
   \ con.f makes only constants, records a warm build installs from its object.
   s" con.f" S\" 3 constant CON-K\nCON-K 1+ constant CON-USE\n" PUT
   \ con-dep.f compiles against con.f's records, installed on a warm build.
   \ It loads con.f itself: an earlier top-level load's records have no key.
   s" con-dep.f" S\" require con.f\n: CON-DEP ( -- n ) CON-USE 1+ ;\n" PUT
   \ rp.f requires rc.f between its two definitions: its object holds two
   \ segments and the load between them.
   s" rc.f" S\" : RC-N ( -- n ) 2 ;\n" PUT
   s" rp.f" S\" : RP-A ( -- n ) 1 ;\nrequire rc.f\n: RP-B ( -- n ) RP-A RC-N + ;\n" PUT
   \ w.f makes a wordlist no package record owns: the next one is W's next.
   s" w.f" S\" wordlist constant W\n3 constant K\n" PUT
   \ tier0.f loads at tier 0, where an install's writers do not run.
   s" tier0.f" S\" 6 constant TIER-K\n" PUT
   \ td.f changes the tier and restores it.
   s" td.f" S\" 0 set-tier\n4 constant TD-K\n1 set-tier\n" PUT
   \ pk.f's `require pkglob.f` is a no-op in pkroot.f's session, so pk.f stays
   \ a leaf, as exec.f does; PK-SEVEN is a sibling child's keyed record.
   s" pkglob.f" S\" : PK-SEVEN ( -- n ) 7 ;\n" PUT
   s" pk.f" S\" require pkglob.f\npackage FRAME-PK\npublic\n: PK-VAL ( -- n ) PK-SEVEN ;\n;package\n" PUT
   s" pkroot.f" S\" require pkglob.f\nrequire pk.f\n" PUT
   s" " s" dup +" s" 3 TWICE drop" LIB-SOURCE
   s" diag.f" S\" : NOCONTRACT-DIAG ( -- ) DIAGXT ;\n" PUT
   s" brokendef.f" S\" 0 set-tier\n: BROKEN ( -- n ) 1 2 ;\n1 set-tier\n" PUT
   s" broken.f" S\" require brokendef.f\n: NOCONTRACT-BROKEN ( -- n ) BROKEN ;\n" PUT
   s" after.f" S\" : AFTER-ONE ( -- n ) 1 ;\n" PUT
   \ aftrun.f's one dependency row is the run of its sibling aftone.f's
   \ AFT-ONE: both leaves load under aftpar.f, since an earlier top-level
   \ load's record has no key aftrun.f's row could name.
   s" aftone.f" S\" : AFT-ONE ( -- n ) 1 ;\n" PUT
   s" aftrun.f" S\" AFT-ONE constant AFTER-K\n" PUT
   s" aftpar.f" S\" require aftone.f\nrequire aftrun.f\n" PUT
   \ alt.f loads ret-use.f under another loader than ret-late.f, and aftone.f
   \ under another than aftpar.f. k.f's CON-K, an earlier top-level load's
   \ record, shares its name with con.f's.
   s" alt.f" S\" : RET-LATE ( -- n ) 5 ;\nrequire ret-use.f\nrequire aftone.f\n" PUT
   s" k.f" S\" 3 constant CON-K\n" PUT
   s" multi.f" S\" : MULTI-NINE ( -- n ) 9 ;\n" PUT
   s" exec.f" S\" require lib.f\nFRAME-LIB:SEVEN constant EXEC-SEVEN\n' FRAME-LIB:TWICE drop\nusing FRAME-LIB\nSEVEN constant EXEC-BARE-SEVEN\n;using\n" PUT
   \ peek.f's TRUSTED: PEEK reads its own data; run.f's top level runs it,
   \ safe.f's runs the checked SAFE-PEEK that calls it, and vread.f's reads
   \ the data word STORE itself. A run that reaches the unchecked PEEK refuses
   \ its file.
   s" peek.f" S\" package FRAME-PEEK\npublic\nvariable STORE\nTRUSTED: PEEK ( -- n ) STORE @ ;\n: SAFE-PEEK ( -- n ) PEEK ;\n;package\n" PUT
   s" run.f" S\" require peek.f\nFRAME-PEEK:PEEK constant RUN-SEEN\n" PUT
   s" safe.f" S\" require peek.f\nFRAME-PEEK:SAFE-PEEK constant SAFE-SEEN\n" PUT
   s" vread.f" S\" require peek.f\nFRAME-PEEK:STORE @ constant STORE-SEEN\n" PUT
   \ cread.f's top level runs its child cpeek.f's checked CPEEK, which reads
   \ CSTORE. peek.f's records belong to top.f's load, which no later file can
   \ name, so the reader is cread.f's own child.
   s" cpeek.f" S\" package FRAME-CPEEK\npublic\nvariable CSTORE\n: CPEEK ( -- n ) CSTORE @ ;\n;package\n" PUT
   s" cread.f" S\" require cpeek.f\nFRAME-CPEEK:CPEEK constant CREAD-SEEN\n" PUT
   \ act-ok.f's run of AK-INIT stores with `!`, whose lowering branches to the
   \ engine helper (PROT-SPAN): the body's MEMORY reason accounts for the store,
   \ so the closure admits the helper. A run that reaches a seed word no REACH
   \ class covers refuses its file: reach.f's TOOL-POKE reaches the compiler's
   \ SESSION-LIVE?, fw-def.f's FW the builder's WRITE-ALL, out.f's OUT-A emit.
   s" act-ok.f" S\" variable AK\n: AK-INIT ( -- ) 7 AK ! ;\nAK-INIT\n3 constant AK-K\n" PUT
   s" tool-def.f" S\" : TOOL-POKE ( -- ) NCOMP:SESSION-LIVE? drop ;\n" PUT
   s" reach.f" S\" require tool-def.f\n1 constant REACH-K\nTOOL-POKE\n" PUT
   0 INPUT-U !
   S\" : FW ( -- ) s\" " IN+ s" fw.txt" AT IN+ S\" \" s\" x\" WRITE-ALL ;\nFW\n" IN+
   s" fw-def.f" INPUT INPUT-U @ PUT
   s" out.f" S\" : OUT-A ( -- ) 65 emit ;\nOUT-A\n" PUT
   \ mask.f's run of MX-RUN names data first, then runs a quotation through
   \ execute: its worst reason, INDIRECT, refuses, not the data mention.
   \ own-trust.f runs its own unchecked OT at top level and is refused by name.
   s" mask.f" S\" variable MX\n: MX-ONE ( -- n ) 1 ;\n: MX-RUN ( -- n ) MX @ drop [: MX-ONE ;] execute ;\nMX-RUN constant MX-K\n" PUT
   s" own-trust.f" S\" TRUSTED: OT ( -- n ) 5 ;\nOT constant OT-K\n" PUT
   \ A seed word run at top level is admitted only by the trace its run leaves.
   \ parse-ok.f's parse-imm adds a unit fact, cells is a stack primitive and
   \ allot a data one; tier-ok.f's set-tier is a build primitive. fwrite.f's
   \ WRITE-ALL, fenv.f's GETENV, dbase.f's data-base, exec-xt.f's execute and
   \ act2.f's SESSION-LIVE? leave no trace. The tick of EX is a resolution,
   \ not a run. nest-parent.f's include opens nest-child.f,
   \ a trace; its SESSION-LIVE? after the child leaves none. act.f closes the
   \ compiler session, so a warm build that installs it instead of running it
   \ prints session=-1. It loads before scope.f, which must stay the last load.
   s" parse-ok.f" S\" : PO ( -- ) ;\ns\" PO\" 0 parse-imm\ncreate PK 4 cells allot\n2 constant PK-K\n" PUT
   s" tier-ok.f" S\" 1 set-tier\n5 constant TO-K\n" PUT
   0 INPUT-U !
   S\" 1 constant FW-K\ns\" " IN+ s" fwrite.txt" AT IN+ S\" \" s\" x\" WRITE-ALL\n" IN+
   s" fwrite.f" INPUT INPUT-U @ PUT
   s" fenv.f" S\" 1 constant FE-K\ns\" HOME\" GETENV nip constant FE-N\n" PUT
   s" dbase.f" S\" 1 constant DB-K\ndata-base DB-K + drop\n" PUT
   \ nestev.f's top level evaluates a text whose seed runs write a kernel cell
   \ below the DATA floor, then define a record: each run the nested text makes
   \ is judged as the file's own. nestdef.f's STRUCTURE evaluates the text its
   \ definer generates, and stores.
   s" nestev.f" S\" s\" data-base ARGC-CELL + @ data-base ARGC-CELL + ! 1 constant NE-K\" evaluate\n" PUT
   s" nestdef.f" S\" STRUCTURE nd-pair DERIVE addr FIELD a n FIELD b n ;STRUCTURE\n" PUT
   \ nest2.f makes nestev.f's store two evaluates deep. nestafter.f's top level
   \ runs a trace-free data-base after a nested evaluate returns: the file's
   \ own tokens are observed again.
   s" nest2.f" S\" S\\\" s\\\" data-base ARGC-CELL + @ data-base ARGC-CELL + ! 1 constant N2-K\\\" evaluate\" evaluate\n" PUT
   s" nestafter.f" S\" s\" 1 constant NA-K\" evaluate\ndata-base NA-K + drop\n" PUT
   \ nestlit.f evaluates the name the engine holds for data-base, a buffer
   \ below the file's source: the nested run's token end is read in its own
   \ buffer, never as an offset into the file's. NL$'s own run reaches a
   \ kernel word, so only the frame's reason tells the two apart.
   s" nestlit.f" S\" : NL$ ( -- ptr u8 n ) s\" data-base\" XREF-FIND XREF-NAME$ ;\nNL$ evaluate drop\n" PUT
   \ nestthrow.f's top-level catch runs a text that allots, then evaluates a
   \ text that throws: the catch restores here, so the trace the abandoned
   \ runs left is gone and they refuse, though here moved before the throw.
   s" nestthrow.f" S\" : NT-Q ( -- ptr u8 n ) s\" 5 throw\" ;\n: NT$ ( -- ptr u8 n ) s\" 8 allot 0 set-tier NT-Q evaluate\" ;\nNT$ ' evaluate catch constant NT-RC 2drop\n1 set-tier\n" PUT
   s" exec-xt.f" S\" : EX ( -- ) ;\n' EX execute\n" PUT
   s" act2.f" S\" NCOMP:SESSION-LIVE? drop\n1 constant ACT-K2\n" PUT
   s" nest-child.f" S\" 2 constant NC-K\n" PUT
   s" nest-parent.f" S\" : NP ( -- ) ;\ninclude nest-child.f\nNCOMP:SESSION-LIVE? drop\n" PUT
   s" act.f" S\" 1 constant ACT-K\nIR-CTX:SESSION-CLOSE\n" PUT
   \ watermark.f moves the user-slot watermark of the TASK the builder loaded at
   \ load time through the cell TASK hands its claimants. TASK:+USER moves it too
   \ and refuses its file for task-slots the same way.
   \ scope.f leaves its package open, the last load: a using it left open
   \ would not count, as usings end with their file (interpret.f CLEAN-END).
   s" watermark.f" S\" : SLOT-BUMP ( ptr u8 -- ) CELL-VIEW CELL swap +! ;\n' SLOT-BUMP TASK:OWNED-USER-NEXT\n" PUT
   s" scope.f" S\" package FRAME-SCOPE\n" PUT
   \ retire.f retires a record lib.f made.
   s" retire.f" S\" undefine FRAME-LIB:BUMP\n" PUT
   \ ret-own.f retires its own record in the segment that made it.
   \ ret-kid.f retires the record its parent ret-parent.f made before loading
   \ it. ret-late.f retires its own record after a child that uses it.
   s" ret-own.f" S\" : RET-OWN ( -- n ) 1 ;\nundefine RET-OWN\n" PUT
   s" ret-kid.f" S\" undefine RET-KID\n" PUT
   s" ret-parent.f" S\" : RET-KID ( -- n ) 2 ;\nrequire ret-kid.f\n" PUT
   s" ret-use.f" S\" : RET-USER ( -- n ) RET-LATE ;\n" PUT
   s" ret-late.f" S\" : RET-LATE ( -- n ) 3 ;\nrequire ret-use.f\nundefine RET-LATE\n" PUT
   \ incut.f cuts back to its own second definition, within its open segment.
   \ gc2.f cuts back to the first definition of gc1.f, which gp.f loaded
   \ before it: below gc2.f's open segment, through gp.f's load of gc1.f.
   \ k1.f cuts back to its own first definition, the ordinal of q1.f's load
   \ of it.
   s" incut.f" S\" : CA ( -- n ) 1 ;\n: CB ( -- n ) 2 ;\ns\" CB\" FORGET-DEFS-FROM\n" PUT
   s" gc1.f" S\" : G1 ( -- n ) 1 ;\n" PUT
   s" gc2.f" S\" s\" G1\" FORGET-DEFS-FROM\n" PUT
   s" gp.f" S\" require gc1.f\nrequire gc2.f\n" PUT
   s" k1.f" S\" : KA ( -- n ) 2 ;\ns\" KA\" FORGET-DEFS-FROM\n: KX ( -- n ) 3 ;\n" PUT
   s" q1.f" S\" : Q1 ( -- n ) 1 ;\nrequire k1.f\n: Q2 ( -- n ) KX ;\n" PUT
   \ raw.f's top level stores RAW-TARGET's code address into the plain variable
   \ RAW-CELL, which declares no xt cell; the tick is a run of the SAFE
   \ RAW-TARGET.
   s" raw.f" S\" variable RAW-CELL\n: RAW-TARGET ( -- ) ;\n' RAW-TARGET RAW-CELL !\n" PUT
   \ poke.f's top level stores BOARD-GO's code address 8 bytes into board.f's
   \ 16-byte BOARD, an earlier cell no address row declares: the update check
   \ refuses it there. board.f loads in poke.f's session, above its image floor.
   s" board.f" S\" create BOARD 16 allot\n: BOARD-GO ( -- ) ;\n" PUT
   s" poke.f" S\" require board.f\n' BOARD-GO BOARD 8 + !\n" PUT
   \ slot.f's declared xt cell holds its child sdeep.f's TEN, which nothing
   \ else in it compiles. A record of an earlier top-level load is no file's
   \ record a later object can name, so each file here that needs a stored
   \ object loads its dependencies in its own session.
   s" sdeep.f" S\" package FRAME-SDEEP\npublic\n: TEN ( -- n ) 10 ;\n;package\n" PUT
   s" slot.f" S\" require sdeep.f\nTYPED-VARIABLE SLOT-HOOK [ -- n ]\n' FRAME-SDEEP:TEN SLOT-HOOK xt!\n" PUT
   \ later.f's declared pointer cell holds an address 8 bytes into earlier.f's
   \ T, reached from U, so no run reads T; T-MID compiles a binding of T.
   s" earlier.f" S\" package FRAME-EARLY\npublic\ncreate T 16 allot\ncreate U 8 allot\n;package\n" PUT
   s" later.f" S\" require earlier.f\nPERSISTED-PTR-VARIABLE LATE-P\n: T-MID ( -- ptr u8 ) FRAME-EARLY:T 8 + ;\nFRAME-EARLY:U 8 - LATE-P !\n" PUT
   \ list.f's DATA holds two addresses of its own DATA; addr.f's holds the
   \ address of its child hits.f's HITS; upd.f's top level stores the address of
   \ its child pina.f's PIN-A into PIN-A, an update, since a load changes only
   \ the earlier DATA of its own session. off.f stores odeep.f's TEN with xt!,
   \ which declares the cell it writes, eight bytes into hooks.f's 16-byte
   \ PIN-HOOKS. PIN-HOOKS is a create, not a TYPED-BUFFER: that definer's
   \ accessor calls the engine's DYNAMIC-STORAGE:BOUNDS, a seed code record no
   \ REACH class covers, so a run of it is refused run-reaches-kernel-word
   \ word=BOUNDS and would order off.f's frame before the slot check. own.f's
   \ DATA holds its child pinb.f's PIN-B, the last record with DATA, and then
   \ an address of its own DATA past PIN-B, which pins own.f's segment as well.
   s" list.f" S\" create PIN-LIST here , here ,\n" PUT
   s" hits.f" S\" package FRAME-HITS\npublic\nvariable HITS\n;package\n" PUT
   s" addr.f" S\" require hits.f\ncreate PIN-HITS FRAME-HITS:HITS ,\n" PUT
   s" pina.f" S\" variable PIN-A\n" PUT
   s" upd.f" S\" require pina.f\nPIN-A PIN-A !\n" PUT
   s" hooks.f" S\" create PIN-HOOKS 16 allot\n" PUT
   s" odeep.f" S\" package FRAME-ODEEP\npublic\n: TEN ( -- n ) 10 ;\n;package\n" PUT
   s" off.f" S\" require hooks.f\nrequire odeep.f\n' FRAME-ODEEP:TEN PIN-HOOKS 8 + xt!\n" PUT
   s" pinb.f" S\" variable PIN-B\n" PUT
   s" own.f" S\" require pinb.f\nPIN-B , here ,\n" PUT
   \ ev.f's records belong to its own top-level load. evw.f's store into EV0,
   \ evx.f's code address in EV-T and evr.f's read of EV0 reach them from
   \ later loads; ev2.f's store into its child ev3.f's EV3 is an update.
   s" ev.f" S\" variable EV0\ncreate EV-T 16 allot\n: EV-GO ( -- ) ;\n" PUT
   s" evw.f" S\" 9 EV0 !\n" PUT
   s" evx.f" S\" TRUSTED: EV-SET ( -- ) ['] EV-GO EV-T 8 + ! ;\nEV-SET\n" PUT
   s" evr.f" S\" EV0 @ constant EV-SEEN\n" PUT
   s" ev3.f" S\" variable EV3\n" PUT
   s" ev2.f" S\" require ev3.f\n9 EV3 !\n" PUT
   \ tier.f leaves the compiler tier at 0; tierback.f restores it before
   \ its definition, since a definition the tier-0 compiler makes is a record
   \ of unknown kind.
   s" tier.f" S\" 0 set-tier\n" PUT
   \ tierpar.f's own segments leave the tier as they found it, but its key's
   \ tier is not the one its child left.
   s" tierpar.f" S\" require tier.f\n" PUT
   s" tierback.f" S\" 0 set-tier\n1 set-tier\n: TIER-T1 ( -- ) ;\n" PUT
   \ pkgc.f closes the package its loader opened. Its load opened inside a
   \ package, so the checker mark refuses it first; scope-exit would too. The
   \ builder made that package between loads, so its frame cannot name it.
   s" pkgc.f" S\" ;package\n" PUT
   \ pkgload.f requires pkgchild.f inside its package: the checker cannot mark
   \ the child's start, and the replay would load the child in the scope
   \ pkgload.f began in, so the load refuses pkgload.f first.
   s" pkgload.f" S\" package FRAME-PKGLOAD\nrequire pkgchild.f\n: INSIDE ( -- n ) 3 ;\n;package\n" PUT
   s" pkgchild.f" S\" : PKG-CHILD ( -- n ) 4 ;\n" PUT
   \ newtype.f declares a type family beside a package record: the record
   \ gives it a segment to install, and the install must import the family.
   s" newtype.f" S\" package FRAME-NEWTYPE\nNEWTYPE frnt\n;package\n" PUT
   FFI-FIXTURES
   NEST-FIXTURES
   \ IMM-USER runs lib.f's immediate MARK while it compiles. app.f loads
   \ exec.f between its own definitions, and its top level runs TALLY, which
   \ adds 1 to lib.f's HITS, then stores lib.f's QUAD into lib.f's declared
   \ xt cell HOOK.
   \ og.f's constants and ogw.f's colon word install on a warm build.
   s" og.f" S\" package FRAME-OG\npublic\n3 constant OG-P\n;package\n5 constant OG-G\n" PUT
   s" ogw.f" S\" : OG-W ( -- n ) 7 ;\n" PUT
   \ dv.f's variable and created table install with their DATA bytes, the
   \ top level's store and commas among them; DV-S's string literal between
   \ them lives in the build's literal pool, not at here. df.f holds a defer
   \ alone.
   s" dv.f" S\" variable DV-V 7 DV-V !\n: DV-S ( -- ptr u8 n ) s\" lit\" ;\ncreate DV-T 3 , 4 ,\n: DV-SUM ( -- n ) DV-V @ DV-T @ + DV-T cell+ @ + ;\n" PUT
   s" df.f" S\" defer DF-D ( -- n )\n" PUT
   s" jrn.f" S\" 3 constant EK-K\n" PUT
   s" pcf.f" S\" 3 constant PC-F3\npackage PCF\npublic\nexport PC-F3\n;package\n" PUT
   s" pcz.f" S\" 3 constant PC-Z3\n" PUT
   s" pcd.f" S\" 3 constant PC-D3\n" PUT
   s" pce.f" S\" 3 constant PC-K3\npackage PCE\npublic\nexport PC-K3\n;package\n" PUT
   s" app.f" APP$ PUT
   s" top.f" S\" require app.f\nrequire check.f\n" PUT
   s" check.f" S\" \\ the first build has no object to check\n" PUT ;

: FRAME-CHECKS ( -- )
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   OUT$ S\" pk=7\n" CONTAINS? TTRUE
   OUT$ s" refused=1" CONTAINS? TTRUE
   OUT$ S\" con=4\n" CONTAINS? TTRUE
   OUT$ S\" dep=5\n" CONTAINS? TTRUE
   OUT$ S\" w=1\nk=3\n" CONTAINS? TTRUE
   s" w.f writes its object" T-LABEL
   ERR$ s" /w.f key=" CONTAINS? TTRUE
   s" tier0.f, loaded at tier 0, writes no object" T-LABEL
   OUT$ S\" tier=6\n" CONTAINS? TTRUE
   s" tier0.f" s"  reason=" LINE$ s" tier0.f reason=tier" T$=
   s" td.f restores the tier it changed and writes its object" T-LABEL
   OUT$ S\" td=4\n" CONTAINS? TTRUE
   ERR$ s" /td.f key=" CONTAINS? TTRUE
   s" diag.f binds DIAGXT through its handed-over contract" T-LABEL
   s" diag.f" FRAME$ s"  ordered=0 binding=1 value=0 resolution=2 executed=0 data=0" ENDS-WITH? TTRUE
   s" broken.f binds BROKEN's recovery record and stays ordered" T-LABEL
   \ Its require of brokendef.f is the third resolution row.
   s" broken.f" FRAME$
      s"  ordered=1 binding=0 value=0 resolution=3 executed=0 data=0 reason=binding-contract-unknown word=BROKEN"
      ENDS-WITH? TTRUE
   s" after.f, the first file after the run, stores its first definition" T-LABEL
   s" after.f" DEFS$ s" 1" T$=
   s" app.f consumes ONE, TWICE, TEN, SEVEN, HITS and MARK once each, SEVEN's value, MARK's run, HITS's data through TALLY's run and HOOK's run and data" T-LABEL
   \ The xt! line adds 5 resolution rows (FRAME-LIB:QUAD and FRAME-LIB:HOOK 2
   \ each, namespace and public, xt! 1 global), HOOK's run and its generated
   \ body's, and HOOK's data.
   s" app.f" FRAME$ s"  ordered=0 binding=6 value=1 resolution=34 executed=3 data=2" ENDS-WITH? TTRUE
   s" lib.f consumes ONE only, none of its own or deep.f's records" T-LABEL
   \ A lookup in FRAME-LIB adds none: lib.f made the package and loads no child
   \ after it, so its own source decides every public and private hit or miss.
   \ That FRAME-LIB did not exist before lib.f is its `package` decision, 1 row.
   s" lib.f" FRAME$ s"  ordered=0 binding=1 value=0 resolution=20 executed=0 data=0" ENDS-WITH? TTRUE
   \ exec.f compiles nothing, so every resolution row is a run's: require and
   \ drop 1 each (global), FRAME-LIB:SEVEN and the ticked FRAME-LIB:TWICE 2 each
   \ (namespace, public), SEVEN under using 3 (namespace, public, global miss),
   \ and `using FRAME-LIB`'s decision 1, keyed to lib.f's record. A run that
   \ recorded only the leg holding its record would leave 6.
   s" exec.f only runs: SEVEN's value, TWICE's run and every leg of each run's resolution" T-LABEL
   s" exec.f" FRAME$ s"  ordered=0 binding=0 value=1 resolution=10 executed=1 data=0" ENDS-WITH? TTRUE
   s" run.f's run of the trusted PEEK refuses it" T-LABEL
   s" run.f" FRAME$ s"  reason=run-host-unchecked word=PEEK" ENDS-WITH? TTRUE
   s" run.f" NO-OBJECT
   s" safe.f's run of the checked SAFE-PEEK reaches PEEK and refuses it" T-LABEL
   s" safe.f" FRAME$ s"  reason=run-host-unchecked word=PEEK" ENDS-WITH? TTRUE
   s" safe.f" NO-OBJECT
   s" vread.f reads the data word STORE at top level" T-LABEL
   s" vread.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" vread.f" FRAME$ s"  executed=" FIELD$ s" 0" T$=
   s" vread.f" FRAME$ s"  data=" FIELD$ s" 1" T$= ;

: OBJECT-CHECKS ( -- )
   s" each file object holds its colon definitions, TRUSTED: BUMP included" T-LABEL
   ERR$ s" native-file: reused" CONTAINS? TFALSE
   s" con.f" s"  records=" s" 2" OBJECT-HAS
   s" base.f" DEFS$ s" 1" T$=
   s" deep.f" DEFS$ s" 1" T$=
   s" app.f" DEFS$ s" 4" T$=
   s" lib.f" DEFS$ s" 5" T$=
   s" each file object holds exactly its own input rows" T-LABEL
   s" base.f" s" 1" INPUTS
   s" deep.f" s" 1" INPUTS
   s" lib.f" s" 4" INPUTS
   s" app.f" s" 13" INPUTS
   s" each object is stored under its key" T-LABEL
   s" app.f" KEY$ APP-KEY KEY-LEN KEEP
   s" lib.f" KEY$ LIB-KEY KEY-LEN KEEP
   s" ret-use.f" KEY$ RU-KEY KEY-LEN KEEP
   s" app.f" KEY$ STORED
   s" lib.f's object holds its records, HITS, T and HOOK's data and deep.f's load" T-LABEL
   s" lib.f" s"  records=" s" 10" OBJECT-HAS
   \ DATA: HITS 8 bytes, T 16, HOOK 8, and the 14 bytes of "FRAME-LIB:MARK"
   \ that its interpreted s" copies to HERE.
   s" lib.f" s"  data=" s" 46" OBJECT-HAS
   s" lib.f" s"  requires=" s" 1" OBJECT-HAS
   s" lib.f" s"  loads=" s" 1" OBJECT-HAS
   s" lib.f" s"  segments=" s" 2" OBJECT-HAS
   s" app.f's object holds six loads, the last between its own definitions" T-LABEL
   s" app.f" s"  records=" s" 4" OBJECT-HAS
   s" app.f" s"  requires=" s" 6" OBJECT-HAS
   s" app.f" s"  loads=" s" 6" OBJECT-HAS
   s" app.f" s"  segments=" s" 7" OBJECT-HAS
   s" base.f" s"  loads=" s" 0" OBJECT-HAS
   s" base.f" s"  segments=" s" 1" OBJECT-HAS
   s" each object holds one checker slice per load and one for itself" T-LABEL
   s" base.f" s"  checks=" s" 1" OBJECT-HAS
   s" lib.f" s"  checks=" s" 2" OBJECT-HAS
   s" app.f" s"  checks=" s" 7" OBJECT-HAS
   s" lib.f's declared xt cell HOOK is its one slot" T-LABEL
   s" lib.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f's store into lib.f's HOOK is its one slot, binding HOOK and QUAD beyond its frame's rows" T-LABEL
   s" app.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" app.f" FRAME$ s"  rows=" FIELD$ FIELD-N 2 + T=
   s" app.f's run of TALLY changes one byte of lib.f's HITS; lib.f writes no earlier DATA" T-LABEL
   s" app.f" s"  updates=" s" 1" OBJECT-HAS
   s" lib.f" s"  updates=" s" 0" OBJECT-HAS
   s" raw.f's undeclared code address refuses its object, naming the cell" T-LABEL
   s" raw.f" s"  reason=" LINE$
      s" raw.f reason=raw-cell owner=RAW-CELL offset=0 value=0x" STARTS-WITH? TTRUE
   ERR$ s" raw.f key=" CONTAINS? TFALSE
   s" poke.f's code address in earlier DATA refuses its object, naming the cell" T-LABEL
   s" poke.f" s"  reason=" LINE$
      s" poke.f reason=raw-cell owner=BOARD offset=8 value=0x" STARTS-WITH? TTRUE
   ERR$ s" poke.f key=" CONTAINS? TFALSE
   s" slot.f's slot names TEN, so its object binds TEN beyond its frame's rows" T-LABEL
   s" slot.f" s"  slots=" s" 1" OBJECT-HAS
   s" slot.f" FRAME$ s"  binding=" FIELD$ s" 0" T$=
   s" slot.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" slot.f" FRAME$ s"  rows=" FIELD$ FIELD-N 1+ T=
   \ The frame binds T and reads U's DATA. The slot adds T's DATA row, whose
   \ digest covers T's extent, so an edit that moves T's end is stale; a
   \ binding of T alone would add no row.
   s" later.f's slot points into T, so its object reads T's DATA beyond its frame's rows" T-LABEL
   s" later.f" s"  slots=" s" 1" OBJECT-HAS
   s" later.f" FRAME$ s"  binding=" FIELD$ s" 1" T$=
   s" later.f" FRAME$ s"  data=" FIELD$ s" 1" T$=
   s" later.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" later.f" FRAME$ s"  rows=" FIELD$ FIELD-N 1+ T=
   s" list.f's addresses of its own DATA pin its one segment, and it stores" T-LABEL
   s" list.f" s"  pins=" s" 1" OBJECT-HAS
   s" list.f" KEY$ STORED
   s" addr.f's raw address of hits.f's HITS pins HITS" T-LABEL
   s" addr.f" s"  pins=" s" 1" OBJECT-HAS
   s" upd.f's update holding PIN-A's address pins PIN-A" T-LABEL
   s" upd.f" OBJECT$ s"  updates=" FIELD$ FIELD-N 0 > TTRUE
   s" upd.f" s"  pins=" s" 1" OBJECT-HAS
   s" lib.f" s"  pins=" s" 0" OBJECT-HAS
   s" own.f's own address after PIN-B's pins its segment beside PIN-B" T-LABEL
   s" own.f" s"  pins=" s" 2" OBJECT-HAS
   s" off.f's slot site eight bytes into PIN-HOOKS refuses its object" T-LABEL
   s" off.f" s"  reason=" LINE$ s" off.f reason=slot-site-offset" T$=
   s" retire.f refuses its object for retiring lib.f's BUMP" T-LABEL
   s" retire.f" s"  reason=" LINE$ s" retire.f reason=retires-earlier word=BUMP" T$=
   s" a file that retires its own record in the segment that made it stores it" T-LABEL
   s" ret-own.f" KEY$ STORED
   s" a child that retires its parent's record refuses both files" T-LABEL
   s" ret-kid.f" s"  reason=" LINE$ s" ret-kid.f reason=retires-earlier word=RET-KID" T$=
   s" ret-kid.f" NO-OBJECT
   s" ret-parent.f" s"  reason=" LINE$ s" ret-parent.f reason=retires-earlier word=RET-KID" T$=
   s" ret-parent.f" NO-OBJECT
   s" a file that retires its record after a child that uses it refuses" T-LABEL
   s" ret-late.f" s"  reason=" LINE$ s" ret-late.f reason=retires-earlier word=RET-LATE" T$=
   s" ret-late.f" NO-OBJECT
   s" a file that cuts within its open segment stores its object" T-LABEL
   s" incut.f" KEY$ STORED
   s" a cut below the innermost file's open segment refuses it and the file that loaded the cut records" T-LABEL
   s" gc2.f" s"  reason=" LINE$ s" gc2.f reason=cut-below-segment" T$=
   s" gc2.f" NO-OBJECT
   s" gp.f" s"  reason=" LINE$ s" gp.f reason=cut-below-segment" T$=
   s" gp.f" NO-OBJECT
   s" a cut at a child's first ordinal refuses the child and the parent whose load it drops" T-LABEL
   s" k1.f" s"  reason=" LINE$ s" k1.f reason=cut-below-segment" T$=
   s" k1.f" NO-OBJECT
   s" q1.f" s"  reason=" LINE$ s" q1.f reason=cut-below-segment" T$=
   s" q1.f" NO-OBJECT
   s" a file that ends in another scope than it began in has no object" T-LABEL
   s" scope.f" s"  reason=" LINE$ s" scope.f reason=scope-exit" T$=
   s" scope.f" NO-OBJECT
   s" a file that takes a TASK user slot has no object" T-LABEL
   s" watermark.f" FRAME$ s"  reason=run-unaccounted word=OWNED-USER-NEXT" ENDS-WITH? TTRUE
   s" watermark.f" s"  reason=" LINE$ s" watermark.f reason=ordered-dependency" T$=
   s" watermark.f" NO-OBJECT
   s" app.f's object holds each definition's name span in app.f" T-LABEL
   APP-SPANS
   s" a file closed inside a multi-error run refuses the checker export" T-LABEL
   s" multi.f" s"  reason=" LINE$ s" multi.f reason=checker-export" T$=
   s" a file loaded inside a package refuses the checker mark" T-LABEL
   s" pkgchild.f" s"  reason=" LINE$ s" pkgchild.f reason=checker-mark" T$=
   s" a file that loads a child in another scope than it began in has no object" T-LABEL
   s" pkgload.f" s"  reason=" LINE$ s" pkgload.f reason=scope-at-load" T$=
   s" pkgload.f" NO-OBJECT ;

\ Effects on state below the file's own DATA: kernel cells, an earlier
\ top-level load's records and its DATA, and the loader's package.
: FLOOR-CHECKS ( -- )
   s" a file that leaves the compiler tier changed refuses, naming the cell" T-LABEL
   s" tier.f" s"  reason=" LINE$ s" tier.f reason=kernel-cell-changed cell=tier" T$=
   s" tier.f" NO-OBJECT
   s" a file whose child leaves the compiler tier changed refuses" T-LABEL
   s" tierpar.f" s"  reason=" LINE$ s" tierpar.f reason=ambient-changed" T$=
   s" tierpar.f" NO-OBJECT
   s" a file that restores the tier it changed stores" T-LABEL
   s" tierback.f" DEFS$ s" 1" T$=
   s" tierback.f" KEY$ STORED
   s" aftrun.f" KEY$ STORED
   s" ev.f" KEY$ STORED
   \ Every store into an earlier load's DATA names its target, and the name
   \ resolves to a record with no key, which orders the file first.
   s" a store into an earlier load's variable refuses" T-LABEL
   s" evw.f" FRAME$ s"  reason=resolution-key-unknown word=EV0" ENDS-WITH? TTRUE
   s" evw.f" s"  reason=" LINE$ s" evw.f reason=ordered-dependency" T$=
   s" evw.f" NO-OBJECT
   \ EV-SET's compiled address of EV-T binds a record with no key, and its
   \ capture cannot import one.
   s" a code address stored into an earlier load's DATA refuses" T-LABEL
   s" evx.f" FRAME$ s"  reason=binding-key-unknown word=EV-T" ENDS-WITH? TTRUE
   s" evx.f" s"  reason=" LINE$ s" evx.f reason=capture-profile-unsupported" T$=
   s" evx.f" NO-OBJECT
   s" a read of an earlier load's variable is an unknown dependency" T-LABEL
   s" evr.f" FRAME$ s"  reason=resolution-key-unknown word=EV0" ENDS-WITH? TTRUE
   s" evr.f" s"  reason=" LINE$ s" evr.f reason=ordered-dependency" T$=
   s" evr.f" NO-OBJECT
   s" a store into a child's variable in the same load is an update" T-LABEL
   s" ev2.f" s"  updates=" s" 1" OBJECT-HAS
   s" a file that closes its loader's package has no object" T-LABEL
   s" pkgc.f" FRAME$ s"  reason=scope-key-unknown word=FRAME-P3" ENDS-WITH? TTRUE
   s" pkgc.f" s"  reason=" LINE$ s" pkgc.f reason=checker-mark" T$=
   s" pkgc.f" NO-OBJECT ;

\ A file's writes to the seed FFI registry: carried rows store. A seed word
\ the top level runs is judged by the trace it leaves, and a selection or
\ scope change is no trace: PROCESS-SYMBOLS and IMAGE-LIFECYCLE:PREPARE
\ refuse their files, and wrb.f refuses at `true`, the first seed word its
\ SELECT line runs. A run that reaches FFI:DECLARE refuses by that reach.
: FFI-CHECKS ( -- )
   s" a FUNCTION: file whose top level runs PROCESS-SYMBOLS refuses" T-LABEL
   OUT$ S\" pid=-1\n" CONTAINS? TTRUE
   s" ffn.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" a file whose run reaches FFI:DECLARE refuses by that reach" T-LABEL
   OUT$ S\" crc=0\n" CONTAINS? TTRUE
   s" ffl.f" s"  reason=" LINE$ s" ffl.f reason=run-reaches-kernel-word word=DECLARE" T$=
   s" ffl.f" NO-OBJECT
   s" a file reusing that library row stores its object" T-LABEL
   OUT$ S\" crc2=0\n" CONTAINS? TTRUE
   s" ffl2.f" KEY$ STORED
   s" a declaring file that runs PROCESS-SYMBOLS refuses; one reusing its row stores" T-LABEL
   s" sel.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" selc.f" KEY$ STORED
   s" a declaration into the bank not selected refuses by its run's reach" T-LABEL
   s" ffb.f" s"  reason=" LINE$ s" ffb.f reason=run-reaches-kernel-word word=DECLARE" T$=
   s" ffb.f" NO-OBJECT
   s" a file that touches no FFI state stores under a selection it cannot key" T-LABEL
   s" plain.f" s"  records=" s" 1" OBJECT-HAS
   s" plain.f" KEY$ STORED
   s" a file whose top level rewrites the selection refuses" T-LABEL
   OUT$ S\" wrb=-1\n0\n0\n" CONTAINS? TTRUE
   s" wrb.f" s"  reason=run-unaccounted word=true" RUN-REFUSED
   s" a file whose top level clears the scope refuses" T-LABEL
   OUT$ S\" wcb=0\n0\n0\n" CONTAINS? TTRUE
   s" wcb.f" s"  reason=run-unaccounted word=PREPARE" RUN-REFUSED ;

\ Neither name's frame nor a refused line of name names a run reason.
: NO-RUN-REASON ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu FRAME$ s"  reason=run-" FIND-SUB MATCH option
     none OF ENDOF
     some OF drop T-FAIL ENDOF
   ;MATCH
   0 INPUT-U !
   s" /" IN+ name nameu IN+ s"  reason=run-" IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF ENDOF
     some OF drop T-FAIL ENDOF
   ;MATCH ;

\ The files whose runs reach only what a closure admits.
: RUNS-ADMITTED ( -- )
   s" exec.f" NO-RUN-REASON
   s" list.f" NO-RUN-REASON
   s" own.f" NO-RUN-REASON
   s" lib.f" NO-RUN-REASON
   s" app.f" NO-RUN-REASON
   s" vread.f" NO-RUN-REASON
   s" addr.f" NO-RUN-REASON
   s" con-dep.f" NO-RUN-REASON
   s" slot.f" NO-RUN-REASON
   s" later.f" NO-RUN-REASON
   s" incut.f" NO-RUN-REASON
   s" ret-own.f" NO-RUN-REASON
   s" cread.f" NO-RUN-REASON
   s" act-ok.f" NO-RUN-REASON
   s" off.f" NO-RUN-REASON
   s" hooks.f" NO-RUN-REASON
   s" raw.f" NO-RUN-REASON
   s" poke.f" NO-RUN-REASON ;

\ What a top-level run reaches: a checked definition and a classed
\ primitive are admitted; a seed word no REACH class covers refuses.
: RUN-CHECKS ( -- )
   s" cread.f runs the checked CPEEK, which reads CSTORE: one run, one data read" T-LABEL
   s" cread.f" RAN
   s" act-ok.f's run reaches the store guard (PROT-SPAN), and it stores" T-LABEL
   s" act-ok.f" KEY$ STORED
   s" act-ok.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a run that reaches a compiler word refuses its file" T-LABEL
   s" reach.f" FRAME$ s"  reason=run-reaches-kernel-word word=SESSION-LIVE?" ENDS-WITH? TTRUE
   s" reach.f" NO-OBJECT
   s" a run that reaches the builder's WRITE-ALL refuses its file and still writes" T-LABEL
   s" fw-def.f" s"  reason=" LINE$ s" fw-def.f reason=run-reaches-kernel-word word=WRITE-ALL" T$=
   s" fw-def.f" NO-OBJECT
   s" fw.txt" AT EXISTS? TTRUE
   s" a run that reaches the unclassed emit refuses its file and still prints" T-LABEL
   s" out.f" s"  reason=" LINE$ s" out.f reason=run-reaches-kernel-word word=emit" T$=
   s" out.f" NO-OBJECT
   OUT$ s" A" CONTAINS? TTRUE
   s" a run whose body names data and then executes refuses by its worst reason" T-LABEL
   s" mask.f" s"  reason=" LINE$ s" mask.f reason=run-host-indirect word=MX-RUN" T$=
   s" mask.f" NO-OBJECT
   s" a run of the file's own unchecked word refuses it by name" T-LABEL
   s" own-trust.f" s"  reason=" LINE$ s" own-trust.f reason=run-host-unchecked word=OT" T$=
   s" own-trust.f" NO-OBJECT
   s" a seed word run whose trace is the source, a unit fact or a class stores" T-LABEL
   s" parse-ok.f" KEY$ STORED
   s" parse-ok.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" tier-ok.f" KEY$ STORED
   s" tier-ok.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a seed word run that leaves no trace refuses its file" T-LABEL
   s" fwrite.f" FRAME$ s"  reason=run-unaccounted word=WRITE-ALL" ENDS-WITH? TTRUE
   s" fwrite.f" NO-OBJECT
   s" fwrite.txt" AT EXISTS? TTRUE
   s" fenv.f" FRAME$ s"  reason=run-unaccounted word=GETENV" ENDS-WITH? TTRUE
   s" fenv.f" NO-OBJECT
   s" dbase.f" FRAME$ s"  reason=run-unaccounted word=data-base" ENDS-WITH? TTRUE
   s" dbase.f" NO-OBJECT
   s" a seed word run in a text the top level evaluates is judged" T-LABEL
   s" nestev.f" FRAME$ s"  reason=run-unaccounted word=data-base" ENDS-WITH? TTRUE
   s" nestev.f" NO-OBJECT
   s" a definer that evaluates its generated text stores its file" T-LABEL
   s" nestdef.f" KEY$ STORED
   s" nestdef.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a seed word run two evaluates deep is judged" T-LABEL
   s" nest2.f" FRAME$ s"  reason=run-unaccounted word=data-base" ENDS-WITH? TTRUE
   s" nest2.f" NO-OBJECT
   s" the top level is judged again after a nested evaluate returns" T-LABEL
   s" nestafter.f" FRAME$ s"  reason=run-unaccounted word=data-base" ENDS-WITH? TTRUE
   s" nestafter.f" NO-OBJECT
   s" a seed word run in a text outside the file's source is judged" T-LABEL
   s" nestlit.f" FRAME$ s"  reason=run-unaccounted word=data-base" ENDS-WITH? TTRUE
   s" nestlit.f" NO-OBJECT
   s" a nested run a throw abandons refuses" T-LABEL
   s" nestthrow.f" FRAME$ s"  reason=run-abandoned" CONTAINS? TTRUE
   s" nestthrow.f" NO-OBJECT
   s" exec-xt.f" FRAME$ s"  reason=run-unaccounted word=execute" ENDS-WITH? TTRUE
   s" exec-xt.f" NO-OBJECT
   s" act2.f" FRAME$ s"  reason=run-unaccounted word=SESSION-LIVE?" ENDS-WITH? TTRUE
   s" act2.f" NO-OBJECT
   s" a run that opens a child is its trace; a later run in the parent is judged" T-LABEL
   s" nest-parent.f" FRAME$ s"  reason=run-unaccounted word=SESSION-LIVE?" ENDS-WITH? TTRUE
   s" nest-parent.f" NO-OBJECT
   s" nest-child.f" KEY$ STORED
   s" nest-child.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a run that closes the compiler session refuses its file" T-LABEL
   s" act.f" FRAME$ s"  reason=run-unaccounted word=SESSION-CLOSE" ENDS-WITH? TTRUE
   s" act.f" NO-OBJECT
   OUT$ S\" session=0\n" CONTAINS? TTRUE
   s" no other file's run names a run reason" T-LABEL
   RUNS-ADMITTED ;

\ The full journal's rows from hop.f's load, as a cold load records them:
\ HO-PA, then hoc.f's HO-PC, then HO-PB, each unlocated as og.f's are.
: OJ-LINE$ ( -- ptr u8 n ) S\" oj= HO-PA:0\n0\n HO-PC:0\n0\n HO-PB:0\n0\n;oj\n" ;

\ The origin rows PROBE printed, up to their end mark.
: ORIGINS$ ( -- ptr u8 n )
   OUT$ {: out:ptr outu:n :}
   out outu s" origins=" FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        out at +  outu at -  {: a:ptr u:n :}
        a u s" ;origins" FIND-SUB MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

\ A warm build installed name's object: its reused line, the only one with
\ " records=" right after the path, and no object written for it.
: REUSED ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s"  records=" LINE$ {: line:ptr lineu:n :}
   0 INPUT-U !
   name nameu IN+ s"  records=" IN+
   line lineu INPUT INPUT-U @ STARTS-WITH? TTRUE
   name nameu NO-OBJECT ;

\ name loaded where a journal records outside every sink: the open refuses
\ it, so it installs no object and writes none, and its load records the
\ rows (rows, OUT$'s line) a cold load records.
: SINK-REFUSED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n rows:ptr rowsu:n :}
   OUT$ rows rowsu CONTAINS? TTRUE
   0 INPUT-U !
   s" /" IN+ name nameu IN+ s"  records=" IN+
   ERR$ INPUT INPUT-U @ CONTAINS? TFALSE
   \ LINE$ builds its pattern in INPUT; the line it returns lies in ERR$.
   name nameu s"  reason=" LINE$ {: line:ptr lineu:n :}
   0 INPUT-U !
   name nameu IN+ s"  reason=origin-sink" IN+
   line lineu INPUT INPUT-U @ T$=
   name nameu NO-OBJECT ;

: SINK-CHECKS ( -- )
   s" pcf.f" S\" pcf=1\n" SINK-REFUSED
   s" pcz.f" S\" pcz=0\n" SINK-REFUSED
   s" pcd.f" S\" pcd=1\n" SINK-REFUSED
   s" pce.f" S\" pce=1\n" SINK-REFUSED ;


\ The second build over the same cache runs as the first and installs the
\ object of each file whose records it can install.
: WARM-CHECKS ( -- )
   s" probe-warm-out.log" OUT$ PUT
   s" probe-warm-err.log" ERR$ PUT
   s" a warm build runs as the cold one and reuses con.f's object" T-LABEL
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   OUT$ s" refused=1" CONTAINS? TTRUE
   OUT$ S\" con=4\n" CONTAINS? TTRUE
   HITS$ HITS-AT HITS-AT-U @ T$=
   s" con.f" REUSED
   s" a warm build reuses every leaf object of constant and colon records" T-LABEL
   OUT$ S\" pk=7\n" CONTAINS? TTRUE
   s" base.f" REUSED s" deep.f" REUSED s" sdeep.f" REUSED s" odeep.f" REUSED
   s" pk.f" REUSED s" vread.f" REUSED s" exec.f" REUSED
   s" ret-use.f" REUSED s" gc1.f" REUSED s" diag.f" REUSED
   s" after.f" REUSED s" tierback.f" REUSED
   \ aftrun.f's stored executed row over AFT-ONE compares the digest the
   \ cold compile bound with the one the warm install of aftone.f binds.
   s" aftone.f" REUSED s" aftrun.f" REUSED
   s" a warm build installs newtype.f with the type family it declares" T-LABEL
   s" newtype.f" REUSED
   s" con-dep.f, which requires con.f, replays its object over con.f's installed records" T-LABEL
   OUT$ S\" dep=5\n" CONTAINS? TTRUE
   s" con-dep.f" REUSED
   s" a warm build replays rp.f, which requires rc.f between its definitions" T-LABEL
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   s" rc.f" REUSED s" rp.f" REUSED
   s" a warm build compiles w.f, whose wordlist no package record owns" T-LABEL
   OUT$ S\" w=1\nk=3\n" CONTAINS? TTRUE
   ERR$ s" /w.f records=" CONTAINS? TFALSE
   s" a warm build compiles tier0.f at tier 0 again" T-LABEL
   OUT$ S\" tier=6\n" CONTAINS? TTRUE
   ERR$ s" /tier0.f records=" CONTAINS? TFALSE
   s" tier0.f" s"  reason=" LINE$ s" tier0.f reason=tier" T$=
   s" a warm build installs td.f" T-LABEL
   OUT$ S\" td=4\n" CONTAINS? TTRUE
   s" td.f" REUSED
   s" a warm build installs dv.f's variable, table and DATA bytes" T-LABEL
   OUT$ S\" dv=14\n" CONTAINS? TTRUE
   OUT$ S\" dvs=lit\n" CONTAINS? TTRUE
   s" dv.f" REUSED
   s" a warm build compiles df.f again: a defer's dispatch cell is a slot" T-LABEL
   s" df.f" s"  reason=" LINE$ s" df.f reason=unsupported-slots" T$=
   s" a warm build installs og.f and holds the cold build's origin rows" T-LABEL
   s" og.f" REUSED
   ORIGINS$ OG-LINE OG-LINE-U @ T$=
   s" a warm build installs jrn.f and imports its early origin row" T-LABEL
   s" jrn.f" REUSED
   OUT$ S\" ek=1\n" CONTAINS? TTRUE
   s" a warm build installs hop.f and hoc.f and imports their origin rows in the cold order" T-LABEL
   s" hop.f" REUSED s" hoc.f" REUSED
   OUT$ OJ-LINE$ CONTAINS? TTRUE
   s" a warm build installs the files loaded where no journal records" T-LABEL
   OUT$ S\" pcf=0\n" CONTAINS? TTRUE
   OUT$ S\" pcz=0\n" CONTAINS? TTRUE
   OUT$ S\" pcd=0\n" CONTAINS? TTRUE
   OUT$ S\" pce=0\n" CONTAINS? TTRUE
   s" pcf.f" REUSED s" pcz.f" REUSED s" pcd.f" REUSED s" pce.f" REUSED
   s" a warm build installs the stored FFI files, replaying their rows and selection" T-LABEL
   OUT$ S\" pid=-1\n" CONTAINS? TTRUE
   OUT$ S\" crc=0\n" CONTAINS? TTRUE
   s" ffl2.f" REUSED
   OUT$ S\" crc2=0\n" CONTAINS? TTRUE
   s" selc.f" REUSED
   s" plain.f" REUSED
   s" a warm build refuses each refused FFI file again by its run reason" T-LABEL
   s" ffn.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" sel.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" selb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" scopeb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" wrb.f" s"  reason=run-unaccounted word=true" RUN-REFUSED
   s" wcb.f" s"  reason=run-unaccounted word=PREPARE" RUN-REFUSED
   s" a warm build runs act.f again, which closes the compiler session" T-LABEL
   s" act.f" NO-OBJECT
   OUT$ S\" session=0\n" CONTAINS? TTRUE
   s" a warm build names no run reason for a file whose runs are admitted" T-LABEL
   RUNS-ADMITTED ;

\ A changed constant leaves con.f's object supported but stale: the warm build
\ checks it, compiles con.f again and rewrites the object, kept as con-warm.obj.
: STALE-CON ( -- )
   s" con.f" S\" 4 constant CON-K\nCON-K 1+ constant CON-USE\n" PUT
   s" a warm build compiles a stale con.f again" T-LABEL
   PROBE
   s" probe-stale-out.log" OUT$ PUT
   s" probe-stale-err.log" ERR$ PUT
   SUCCESS
   OUT$ S\" con=5\n" CONTAINS? TTRUE
   ERR$ s" /con.f records=" CONTAINS? TFALSE
   s" con.f" KEY$ s" con-warm.obj" KEEP-OBJ ;

\ The freshness check left no row: a cold build of the edited con.f writes
\ the object the stale warm build rewrote, kept as con-cold.obj.
: COLD-CON ( -- )
   s" a cold build writes the stale build's con.f object" T-LABEL
   s" con.f" KEY$ s" con-cold.obj" KEEP-OBJ
   s" con-warm.obj" AT {: warm:ptr warmp:n :}
   warm warmp FILE-SIZE {: warmu:n :}
   warmu WARM-RESERVE
   warm warmp 0 WARM BYTE-VIEW warmu READ-ALL warmu <> if E-FS-IO throw then
   s" con-cold.obj" AT {: cold:ptr coldp:n :}
   cold coldp FILE-SIZE {: coldu:n :}
   coldu OBJ-RESERVE
   cold coldp 0 OBJ BYTE-VIEW coldu READ-ALL coldu <> if E-FS-IO throw then
   0 OBJ BYTE-VIEW coldu 0 WARM BYTE-VIEW warmu T$= ;

\ ret-use.f's object names its place under ret-late.f, so under alt.f it is
\ no hit: it compiles and writes a second object under its own key. So does
\ aftone.f, whose stored declaration key names aftpar.f: a hit there could
\ not bind it. con.f's object is fresh under con-dep.f, but k.f made CON-K
\ live first: a named miss, and the live compile refuses the duplicate. The
\ warm build after it still reuses all three objects.
: OTHER-LOADER ( -- )
   s" another loader's leaves compile; a live CON-K makes con.f a named miss" T-LABEL
   SESSION
   s" alt.f" LOAD+ s" k.f" LOAD+ s" con-dep.f" LOAD+
   RUN-INPUT
   s" other-loader-out.log" OUT$ PUT
   s" other-loader-err.log" ERR$ PUT
   78 T=
   0 INPUT-U !
   s" duplicate definition: CON-K at " IN+ s" con.f" AT IN+ s" :1" IN+
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   s" con.f" s"  reason=" LINE$ s" con.f reason=name-live word=CON-K" T$=
   ERR$ s" /con.f records=" CONTAINS? TFALSE
   ERR$ s" /ret-use.f records=" CONTAINS? TFALSE
   s" ret-use.f" KEY$ RU-KEY KEY-LEN STR= TFALSE
   ERR$ s" /aftone.f key=" CONTAINS? TTRUE ;

\ ret-late.f defines RET-USER before it requires ret-use.f, whose fresh
\ object would define it: a named miss, and the live compile refuses the
\ duplicate at ret-use.f's definition.
: LOADER-FIRST ( -- )
   s" ret-late.f" S\" : RET-LATE ( -- n ) 3 ;\n: RET-USER ( -- n ) 4 ;\nrequire ret-use.f\nundefine RET-LATE\n" PUT
   s" a colon object whose name its loader defined first is a named miss" T-LABEL
   SESSION
   s" ret-late.f" LOAD+
   RUN-INPUT
   s" loader-first-out.log" OUT$ PUT
   s" loader-first-err.log" ERR$ PUT
   78 T=
   0 INPUT-U !
   s" duplicate definition: RET-USER at " IN+ s" ret-use.f" AT IN+ s" :1" IN+
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   s" ret-use.f" s"  reason=" LINE$ s" ret-use.f reason=name-live word=RET-USER" T$=
   ERR$ s" /ret-use.f records=" CONTAINS? TFALSE ;

\ RC-N's new effect leaves rp.f's first segment fresh and its second stale:
\ the replay has installed RP-A and loaded rc.f, so the build stops with
\ E-NUNIT-PROFILE (a stale later segment is not yet cut and compiled).
: RC-INTERFACE ( -- )
   s" rc.f" S\" : RC-N ( -- u8 ) 2 ;\n" PUT
   s" an effect edit of rc.f stops rp.f's replay at its stale second segment" T-LABEL
   PROBE
   s" rc-interface-out.log" OUT$ PUT
   s" rc-interface-err.log" ERR$ PUT
   0 T<>
   ERR$ s" -8597" CONTAINS? TTRUE
   OUT$ s" rp=" CONTAINS? TFALSE ;

\ Keep line in a LINE-CAP buffer and its length in a cell.
: KEEP-LINE ( ptr u8 n ptr u8 ptr n -- )
   {: line:ptr lineu:n buf:ptr bufu:ptr :}
   lineu LINE-CAP > if T-FAIL exit then
   line buf lineu BYTE-COPY lineu bufu ! ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-frame" TREE
   s" cache" AT MAKE-DIR
   FIXTURES
   \ Each build below is its own builder process: a key that stays stable
   \ across separate builds is the boundary under test.
   PROBE {: rc:n :}
   s" probe-out.log" OUT$ PUT
   s" probe-err.log" ERR$ PUT
   rc SUCCESS
   FRAME-CHECKS
   OBJECT-CHECKS
   FLOOR-CHECKS
   FFI-CHECKS
   RUN-CHECKS
   s" lib.f" KEY$ STORED
   s" a cold build runs dv.f and stores its object" T-LABEL
   OUT$ S\" dv=14\n" CONTAINS? TTRUE
   OUT$ S\" dvs=lit\n" CONTAINS? TTRUE
   s" dv.f" KEY$ STORED
   KEEP-OBJECT
   CHECKER
   s" app.f" RESULTS$ APP-RES RES-LEN KEEP
   s" lib.f" RESULTS$ LIB-RES RES-LEN KEEP
   HITS$ {: at:ptr atu:n :}
   atu AT-CAP > if T-FAIL else at HITS-AT atu BYTE-COPY atu HITS-AT-U ! then
   \ A second builder process over the same sources writes the same app.f
   \ object: checker slices must not depend on the process that exported them.
   s" app.f" OBJECT$ APP-LINE APP-LINE-U KEEP-LINE
   s" a cold build records an origin row for each of og.f's and ogw.f's words" T-LABEL
   ORIGINS$ s" :0" CONTAINS? TFALSE
   ORIGINS$ OG-LINE OG-LINE-U KEEP-LINE
   s" a cold build records hop.f's and hoc.f's origin rows in load order" T-LABEL
   OUT$ OJ-LINE$ CONTAINS? TTRUE
   s" a cold build runs hpar.f over hch.f's two children" T-LABEL
   OUT$ S\" hp=1\n1\n2\n" CONTAINS? TTRUE
   s" a cold build records jrn.f's early origin row under the journal's own hook" T-LABEL
   OUT$ S\" ek=1\n" CONTAINS? TTRUE
   s" files loaded where no journal records store their objects" T-LABEL
   OUT$ S\" pcf=0\n" CONTAINS? TTRUE
   OUT$ S\" pcz=0\n" CONTAINS? TTRUE
   OUT$ S\" pcd=0\n" CONTAINS? TTRUE
   OUT$ S\" pce=0\n" CONTAINS? TTRUE
   s" pcf.f" KEY$ STORED s" pcz.f" KEY$ STORED s" pcd.f" KEY$ STORED s" pce.f" KEY$ STORED
   s" con-dep.f" OBJECT$ DEP-LINE DEP-LINE-U KEEP-LINE
   s" a cold build runs rp.f and stores the objects of rp.f and rc.f" T-LABEL
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   s" rp.f" KEY$ STORED s" rc.f" KEY$ STORED
   OTHER-LOADER
   s" a second build writes the same app.f and con-dep.f objects" T-LABEL
   PROBE SUCCESS WARM-CHECKS
   s" app.f" OBJECT$ APP-LINE APP-LINE-U @ T$=
   \ A comment changes no definition's result, inside TWICE's body or as a
   \ line that moves every later definition. base.f's new variable moves HITS
   \ from here on.
   s" base.f" S\" package FRAME-BASE\nvariable GAP\npublic\n: ONE ( -- n ) 1 ;\n;package\n" PUT
   s" pkpre.f" S\" package FRAME-PK\n: PK-SEVEN ( -- n ) 70 ;\n;package\n" PUT
   s" pkroot.f" S\" require pkglob.f\nrequire pkpre.f\nrequire pk.f\n" PUT
   1 MOVED !
   S\" \\ edited\n" s" ( note ) dup +" s" 3 ( note ) TWICE drop" LIB-SOURCE
   \ sel.f now declares getgid: selc.f's stored getuid reuse row no longer
   \ names a live row.
   s" sel.f" S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: SEL-UID getgid ( -- i32 ) ;FUNCTION\n" PUT
   \ sela.f now selects libz, which selb.f selects over; scopea.f's package,
   \ whose wordlist scopeb.f's object names, is now FRAME-SCB.
   s" sela.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\n" PUT
   s" scopea.f" S\" require src/lib/ffi-abi.f\npackage FRAME-SCB\nPROCESS-SYMBOLS\n;package\n" PUT
   \ wra.f now selects libz, which wrb.f's SELECT replaces.
   s" wra.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\n" PUT
   \ wca.f now selects libz, which wcb.f's PREPARE keeps.
   s" wca.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\n" PUT
   \ RC-N's new body keeps its effect: rp.f's rows do not read it.
   s" rc.f" S\" : RC-N ( -- n ) 2 0 + ;\n" PUT
   s" hch.f" S\" \\ edited\nrequire hg1.f\nrequire hg2.f\n" PUT
   s" comments in lib.f keep both keys and both files' results" REBUILT
   s" a comment compiles hch.f between installs of hg1.f and hg2.f inside the installing hpar.f" T-LABEL
   OUT$ S\" hp=1\n1\n2\n" CONTAINS? TTRUE
   ERR$ s" /hch.f records=" CONTAINS? TFALSE
   s" hpar.f" REUSED s" hg1.f" REUSED s" hg2.f" REUSED
   s" a body edit of rc.f compiles rc.f and replays rp.f around it" T-LABEL
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   ERR$ s" /rc.f records=" CONTAINS? TFALSE
   s" rp.f" REUSED
   s" a byte allotted before dv.f misses its object and loads it as a cold build" T-LABEL
   s" dv.f" s"  reason=" LINE$ s" dv.f reason=data-alignment" T$=
   OUT$ S\" dv=14\n" CONTAINS? TTRUE
   s" an earlier file that now makes FRAME-PK refuses pk.f's object: PK-SEVEN binds its private word" T-LABEL
   OUT$ S\" pk=70\n" CONTAINS? TTRUE
   ERR$ s" /pk.f records=" CONTAINS? TFALSE
   ERR$ s" dependency miss: namespace shape changed" CONTAINS? TTRUE
   s" a changed selection is a named miss for the file that consumed it" T-LABEL
   ERR$ s" /selc.f reason=ffi-rows" CONTAINS? TTRUE
   ERR$ s" /selc.f records=" CONTAINS? TFALSE
   s" sel.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" a file that selects the selection it found refuses on every build" T-LABEL
   s" selb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   ERR$ s" /selb.f records=" CONTAINS? TFALSE
   s" a file opening with a removed package's selection refuses on every build" T-LABEL
   s" scopeb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   ERR$ s" /scopeb.f records=" CONTAINS? TFALSE
   s" a SELECT over a changed selection refuses and selects as a cold load" T-LABEL
   s" wrb.f" s"  reason=run-unaccounted word=true" RUN-REFUSED
   ERR$ s" /wrb.f records=" CONTAINS? TFALSE
   OUT$ S\" wrb=-1\n0\n0\n" CONTAINS? TTRUE
   s" a scope clear over a changed library refuses and keeps it as a cold load" T-LABEL
   s" wcb.f" s"  reason=run-unaccounted word=PREPARE" RUN-REFUSED
   ERR$ s" /wcb.f records=" CONTAINS? TFALSE
   OUT$ S\" wcb=0\n0\n1\n" CONTAINS? TTRUE
   s" og.f under another engine root and jrn.f under another hook compile again" T-LABEL
   ERR$ s" /og.f records=" CONTAINS? TFALSE
   ERR$ s" /jrn.f records=" CONTAINS? TFALSE
   OUT$ S\" ek=0\n" CONTAINS? TTRUE
   ORIGINS$ OG-MOVED OG-MOVED-U KEEP-LINE
   s" a file stored where no journal records does not install where one records outside every sink" T-LABEL
   SINK-CHECKS
   s" lib.f" RESULTS$ LIB-RES RES-LEN T$=
   s" every split of the first build's app.f rows still holds" T-LABEL
   SPLIT$ nip 1 > TTRUE
   51 SPLIT-COUNT SPLIT$ nip T=
   STALE-CON
   \ TWICE's new code changes lib.f's results. An empty cache makes app.f
   \ compile again against lib.f's moved code and HITS's moved address, so
   \ new branch words land in its call sites and TALLY captures the new
   \ address; masked, they keep app.f's results. A warm app.f would hand
   \ back its stored bytes and prove nothing about the mask.
   s" cache" AT REMOVE-TREE s" cache" AT MAKE-DIR
   s" " s" 2 *" s" 3 TWICE drop" LIB-SOURCE
   s" a changed TWICE body keeps both keys and app.f's results" REBUILT
   s" a cold build holds the moved og.f's and jrn.f's origin rows" T-LABEL
   ORIGINS$ OG-MOVED OG-MOVED-U @ T$=
   OUT$ S\" ek=0\n" CONTAINS? TTRUE
   s" a cold build refuses the files loaded where a journal records outside every sink" T-LABEL
   SINK-CHECKS
   s" lib.f" RESULTS$ LIB-RES RES-LEN STR= TFALSE
   \ app.f ran MARK, which calls TWICE, and two runs on its HOOK line reach
   \ HOOK's DATA, which holds TWICE: those three executed rows are stale.
   \ Splits before the first hold below and fail above (1), splits past the
   \ last the reverse (2), splits between fail on both sides (0).
   s" app.f's three rows that reach TWICE are stale, and only ranges holding one fail" T-LABEL
   48 HELD-COUNT 3 T=
   49 SPLIT-COUNT 0 > TTRUE
   50 SPLIT-COUNT 0 > TTRUE
   51 SPLIT-COUNT 0 T=
   SPLIT$ s" 21" CONTAINS? TFALSE
   SPLIT$ s" 01" CONTAINS? TFALSE
   SPLIT$ s" 20" CONTAINS? TFALSE
   ERR$ s" dependency miss: executed closure changed" CONTAINS? TTRUE
   s" base.f's new variable moved HITS" T-LABEL
   HITS$ HITS-AT HITS-AT-U @ STR= TFALSE
   COLD-CON
   LOADER-FIRST
   RC-INTERFACE
   T-REPORT
   s" file frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-FRAME-TEST:MAIN
