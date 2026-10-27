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
require test/ffi-struct-fixture.f

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
\ Nonzero on the second build only: pf.f loads before lp.f.
variable FILL-POOL

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
   S\" require base.f\nrequire run.f\nrequire safe.f\nrequire vread.f\n: PRE ( -- n ) FRAME-BASE:ONE ;\nrequire lib.f\n: IMM-USER ( -- n ) FRAME-LIB:MARK 5 ;\n: FRAME-USE ( -- n )\n   FRAME-LIB:SEVEN FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-BASE:ONE + FRAME-DEEP:TEN + ;\nrequire exec.f\n: TALLY ( -- ) 1 FRAME-LIB:HITS +! ;\n' FRAME-LIB:QUAD FRAME-LIB:HOOK xt!\n" ;

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

\ A forward keeps the fault its device latched: PDEV's write latches fault 5,
\ then forwards to the terminal, whose fault is none. fwd= prints the first
\ write's code, PDEV's fault after it, and the second write's code less
\ E-GENIO-IO. Kept apart from PROBE for its body text limit (BODYBUF-CAP).
: FWD-CASE ( -- )
   S\" package FRAME-FWD\npublic\n: P-WRITE ( ptr u8 n -- ) 5 GENIO:FAULT! GENIO:TERMINAL GENIO:FORWARD ;\n: PDEV ( -- device ) [: FRAME-DEV:NO-EMIT ;] [: FRAME-DEV:NO-KEY ;] [: FRAME-DEV:NO-READY ;] [: FRAME-DEV:NO-READ ;] [: P-WRITE ;] [: FRAME-DEV:NO-READ ;] [: FRAME-DEV:NOTHING ;] [: FRAME-DEV:NOTHING ;] 0 GENIO:DEVICE ;\n;package\nFRAME-FWD:PDEV GENIO:OUTPUT! FRAME-DEV:WCODE GENIO:OUTPUT@ GENIO:FAULT@ FRAME-DEV:WCODE GENIO:OUTPUT@ GENIO:TERMINAL GENIO:OUTPUT! GENIO:CLOSE s\" fwd=\" type rot . swap . GENIO:E-IO - . cr\n" IN+ ;

\ ffo.f, ffg.f, ffe.f and ffx.f run inside an open FUNCTION: declaration.
\ Kept apart from PROBE for its body text limit (BODYBUF-CAP).
: DECL-PROBE ( -- )
   s" ffo.f" LOAD+
   s" ffg.f" LOAD+
   s" ffe.f" LOAD+
   s" ffx.f" LOAD+ ;

\ ffs.f's struct-by-value call prints its checksum, on macOS only, as
\ FFS-FIXTURE writes it; ffsl.f loads first to grow the library table.
\ Kept apart from PROBE for its body text limit (BODYBUF-CAP).
: FFS-PROBE ( -- )
   HB-TARGET-MACOS? if
      s" ffsl.f" LOAD+
      s" ffs.f" LOAD+
      S\" s\" ffs=\" type FFS-S FFS-SUM24 . cr\n" IN+
   then ;

\ The package and bare wordlist shapes load after w.f.
: WL-PROBE ( -- )
   s" wlp.f" LOAD+
   S\" s\" wlp=\" type WLP:A WLP:B + . cr\n" IN+
   s" wlr.f" LOAD+
   S\" s\" wlr=\" type WLQ:A WLQ:B + . cr\n" IN+
   s" wls.f" LOAD+
   S\" s\" wls=\" type WLS:A WLT-K + . cr\n" IN+
   s" wlk.f" LOAD+
   S\" s\" wlx=\" type WLX-CUR WLK-W = bool>n . cr\n" IN+
   s" wld.f" LOAD+
   S\" s\" wlm=\" type WLM-B WLM-A - . WLC-W WLM-A - . WMB-CUR WLM-B = bool>n . WMA-CUR WLM-A = bool>n . WMC-CUR WLC-W = bool>n . cr\n" IN+
   \ The warm build loads wln.f under the other top-level wordlist.
   S\" wordlist constant WLN-A\nwordlist constant WLN-B\n" IN+
   FILL-POOL @ if S\" WLN-A set-current\n" else S\" WLN-B set-current\n" then IN+
   s" wln.f" LOAD+
   S\" 0 set-current\ns\" wln=\" type WLN-CUR WLN-A = bool>n . WLN-CUR WLN-B = bool>n . cr\n" IN+ ;

\ The loads: lbv.f and lbc.f load where no edge can stand; nproot.f loads
\ npk.f, then npc.f, whose own require of npk.f finds it loaded; nreq.f's
\ required of npk.f does the same; evreq.f requires npk.f from an evaluated
\ text; nlroot.f loads the second pair. Kept apart from PROBE for its body
\ text limit (BODYBUF-CAP).
: EDGE-PROBE ( -- )
   s" lbv.f" LOAD+
   S\" s\" lbv=\" type LBV-K LBV-KID + . cr\n" IN+
   s" lbc.f" LOAD+
   S\" s\" lbc=\" type LBC-A LBC-B + . cr\n" IN+
   s" nproot.f" LOAD+
   S\" s\" npc=\" type NPC . cr\n" IN+
   s" nreq.f" LOAD+
   S\" s\" nreq=\" type NREQ . cr\n" IN+
   s" evreq.f" LOAD+
   s" nlroot.f" LOAD+ ;

\ The loads of the INDIRECT-FIXTURES runs that store, each followed by the
\ value it computed.
\ Kept apart from PROBE for its body text limit (BODYBUF-CAP).
: INDIRECT-PROBE ( -- )
   s" mxp.f" LOAD+
   S\" s\" mx=\" type MX-K . cr\n" IN+
   s" xpar.f" LOAD+
   S\" s\" xk=\" type X-K . cr\n" IN+
   s" ixp.f" LOAD+
   S\" s\" ixl=\" type IXL-K . cr\ns\" ixt=\" type IXT-K . cr\n" IN+
   s" dxpar.f" LOAD+
   S\" s\" dxk=\" type DX-K . cr\n" IN+
   s" uhp.f" LOAD+
   S\" s\" uh=\" type UH-K . cr\ns\" uhi=\" type UHI-K . cr\n" IN+
   s" pnt.f" LOAD+ ;

\ The loads of the CAST-FIXTURES runs, each run that stores followed by the
\ value it computed.
\ Kept apart from PROBE for its body text limit (BODYBUF-CAP).
: CAST-PROBE ( -- )
   s" cid.f" LOAD+
   S\" s\" cid=\" type CID-K . cr\n" IN+
   s" rolecast.f" LOAD+
   S\" s\" role=\" type ROLE-SEEN . cr\n" IN+
   s" mint.f" LOAD+
   s" pb.f" LOAD+ ;

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
   \ The moved builds allot a byte before dv.f. The loader aligns HERE when a
   \ file starts, so dv.f's DATA bytes match its stored object, which installs.
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
   s" sx.f" LOAD+
   S\" s\" sx=\" type SX-B . cr\n" IN+
   EDGE-PROBE
   s" hpar.f" LOAD+
   S\" s\" hp=\" type FRAME-HP:B . HG-ONE . HG-TWO . cr\n" IN+
   s" w.f" LOAD+
   S\" s\" w=\" type wordlist W - . s\" k=\" type K . cr\n" IN+
   WL-PROBE
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
   s" rawp.f" LOAD+
   s" pokep.f" LOAD+
   s" slot.f" LOAD+
   s" later.f" LOAD+
   S\" check@ 0 set-check\n" IN+
   s" list.f" LOAD+
   S\" set-check\n" IN+
   s" addrp.f" LOAD+
   s" updp.f" LOAD+
   s" offp.f" LOAD+
   s" ownp.f" LOAD+
   s" ev.f" LOAD+
   s" evw.f" LOAD+
   s" evx.f" LOAD+
   s" evr.f" LOAD+
   s" ev2.f" LOAD+
   s" ev4.f" LOAD+
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
   S\" s\" pid=\" type FF-PID 0 > bool>n . cr\n" IN+
   DECL-PROBE
   FFS-PROBE
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
   S\" s\" wrb=\" type FFI-DECL:SELECTION bool>n . . . cr\n" IN+
   s" wca.f" LOAD+
   s" wcb.f" LOAD+
   S\" s\" wcb=\" type FFI-DECL:SELECTION bool>n . . . cr\n" IN+
   S\" package FRAME-P3\n" IN+
   s" pkgc.f" LOAD+
   s" cread.f" LOAD+
   s" act-ok.f" LOAD+
   s" exit.f" LOAD+
   s" fw-def.f" LOAD+
   s" out.f" LOAD+
   s" reach.f" LOAD+
   INDIRECT-PROBE
   s" rk-ind.f" LOAD+
   s" rk-base.f" LOAD+
   s" rk-rev.f" LOAD+
   s" rk-trust.f" LOAD+
   s" rk-is.f" LOAD+
   s" rk-div.f" LOAD+
   s" rk-mod.f" LOAD+
   s" db-a.f" LOAD+
   s" db-b.f" LOAD+
   s" rebind.f" LOAD+
   s" own-trust.f" LOAD+
   CAST-PROBE
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
   s" ntp.f" LOAD+
   s" exec-xt.f" LOAD+
   s" act2.f" LOAD+
   s" nest-parent.f" LOAD+
   s" act.f" LOAD+
   S\" s\" session=\" type NCOMP:SESSION-LIVE? bool>n . cr\n" IN+
   s" lf.f" LOAD+
   S\" s\" lf=\" type LF-S type cr\n" IN+
   s" rw.f" LOAD+
   S\" s\" rw=\" type RW-GAP . cr\n" IN+
   s" rwp.f" LOAD+
   S\" s\" rwp=\" type RWP-B NULL-PTR - RWP-A NULL-PTR - - . cr\n" IN+
   \ The tee in front of a non-terminal device. Each device logs every
   \ operation it is handed, its kind and length, on the terminal (an emit or
   \ type inside an operation reaches it), and keeps no state, as a write to its
   \ DATA would refuse the printing file: LDEV logs writes and emits, FDEV
   \ logs a write and latches fault 5, XDEV's flush latches fault 5 and throws
   \ 123. The lines from <tee to tee> are the cases' whole output, the same
   \ cold and warm but for the reused counts. pout.f and say.f print through
   \ LDEV. fx.f prints through FDEV: fpar.f reads the tee's fault after it,
   \ and a GENIO:WRITE there and in the session raises E-GENIO-IO. fq.f opens
   \ the tee over FDEV already faulted; fy.f prints under it. fl.f flushes
   \ XDEV through the tee. cap.f's capture fails on BOOM after the tee's own
   \ cleanup, and capo.f prints through the next tee.
   S\" package FRAME-DEV\nvariable BOOMED\npublic\n: L-WRITE ( ptr u8 n -- ) {: b:ptr u:n :} s\" [W\" type u . b u type s\" ]\" type ;\n: L-EMIT ( n -- ) s\" [E\" type . s\" ]\" type ;\n: F-WRITE ( ptr u8 n -- ) nip s\" [F\" type . s\" ]\" type 5 GENIO:FAULT! ;\n: X-FLUSH ( -- ) 5 GENIO:FAULT! 123 throw ;\n: NO-EMIT ( n -- ) drop ;\n: NO-KEY ( -- n ) 0 ;\n: NO-READY ( -- bool ) false ;\n: NO-READ ( ptr u8 n -- n ) drop drop 0 ;\n: NOTHING ( -- ) ;\n: LDEV ( -- device ) [: L-EMIT ;] [: NO-KEY ;] [: NO-READY ;] [: NO-READ ;] [: L-WRITE ;] [: NO-READ ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;\n: FDEV ( -- device ) [: NO-EMIT ;] [: NO-KEY ;] [: NO-READY ;] [: NO-READ ;] [: F-WRITE ;] [: NO-READ ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;\n: XDEV ( -- device ) [: L-EMIT ;] [: NO-KEY ;] [: NO-READY ;] [: NO-READ ;] [: L-WRITE ;] [: NO-READ ;] [: X-FLUSH ;] [: NOTHING ;] 0 GENIO:DEVICE ;\n: WCODE ( -- n ) [: s\" w\" GENIO:WRITE ;] catch ;\n: BOOM ( -- ) BOOMED @ 0<> if exit then 1 BOOMED ! 99 throw ;\n: ARM ( -- ) [: BOOM ;] IMAGE-LIFECYCLE:REGISTER ;\n;package\nGENIO:TERMINAL GENIO:OUTPUT! s\" <tee\" type cr\nFRAME-DEV:LDEV GENIO:OUTPUT!\n" IN+
   s" pout.f" LOAD+
   S\" GENIO:TERMINAL GENIO:OUTPUT!\nFRAME-DEV:FDEV GENIO:OUTPUT!\n" IN+
   s" fpar.f" LOAD+
   S\" GENIO:OUTPUT@ GENIO:FAULT@ FRAME-DEV:WCODE GENIO:TERMINAL GENIO:OUTPUT! s\" fault=\" type swap . s\" wcode=\" type GENIO:E-IO - . s\" fpar=\" type FPAR-FAULT . FPAR-CODE GENIO:E-IO - . cr\nFRAME-DEV:FDEV GENIO:OUTPUT! s\" x\" type\n" IN+
   s" fq.f" LOAD+
   S\" GENIO:TERMINAL GENIO:OUTPUT! s\" fq=\" type FQ-FAULT . FQ-CODE GENIO:E-IO - . cr\nFRAME-DEV:XDEV GENIO:OUTPUT!\n" IN+
   s" fl.f" LOAD+
   S\" GENIO:TERMINAL GENIO:OUTPUT! s\" fl=\" type FL-CODE . FL-FAULT . FL-WCODE GENIO:E-IO - . cr\nIMAGE-LIFECYCLE:PREPARE\nFRAME-DEV:LDEV GENIO:OUTPUT!\nFRAME-DEV:ARM\n" IN+
   s" cap.f" LOAD+
   S\" s\" s\" type cr\n" IN+
   s" capo.f" LOAD+
   S\" GENIO:TERMINAL GENIO:OUTPUT! s\" cap=\" type CAP-CODE . cr s\" tee>\" type cr\n" IN+
   FWD-CASE
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
   \ The pool-full stage loads last but for scope.f, whose open scope refuses
   \ every file after it: pf.f moves DATA for every later file.
   FILL-POOL @ if s" pf.f" LOAD+ then
   s" lp.f" LOAD+
   S\" s\" lp=\" type LP-S type cr\n" IN+
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

\ The session's stdout from its "<tee" line through its "tee>" line.
: TEE$ ( -- ptr u8 n )
   OUT$ S\" <tee\n" FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        OUT$ drop at +  OUT$ nip at -  {: a:ptr u:n :}
        a u S\" tee>\n" FIND-SUB MATCH option
          none OF T-FAIL s" " ENDOF
          some OF IDX>N 5 + a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

\ TEE$ on the cold build: each operation as its device logs it, kind and
\ length, with each top-level load's count report between the cases.
: TEE-COLD ( -- ptr u8 n )
   S\" <tee\n[W1\na][W1\n\n][W4\nsaid][W1\n\n][E66\n][W1\nc][W1\n\n][W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n0\n][F2\n][F1\n][F13\n][F2\n][F10\n][F2\n]fault=5\nwcode=0\nfpar=5\n0\n\n[F1\n][F1\n][F1\n][F13\n][F2\n][F10\n][F2\n]fq=5\n0\n\n[W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n2\n]fl=123\n5\n0\n\n[W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n1\n][W1\ns][W1\n\n][W1\no][W1\n\n][W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n0\n]cap=99\n\ntee>\n" ;

\ TEE$ on the warm build: the same operations, with the warm counts.
: TEE-WARM ( -- ptr u8 n )
   S\" <tee\n[W1\na][W1\n\n][W4\nsaid][W1\n\n][E66\n][W1\nc][W1\n\n][W13\nfiles reused ][W2\n2\n][W10\n compiled ][W2\n0\n][F2\n][F1\n][F13\n][F2\n][F10\n][F2\n]fault=5\nwcode=0\nfpar=5\n0\n\n[F1\n][F1\n][F1\n][F13\n][F2\n][F10\n][F2\n]fq=5\n0\n\n[W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n2\n]fl=123\n5\n0\n\n[W13\nfiles reused ][W2\n0\n][W10\n compiled ][W2\n1\n][W1\ns][W1\n\n][W1\no][W1\n\n][W13\nfiles reused ][W2\n1\n][W10\n compiled ][W2\n0\n]cap=99\n\ntee>\n" ;

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

\ check.f, which top.f requires after app.f and tally.f, checks the first
\ build's app.f dependency rows, kept in app-cold.obj, while the session that
\ loaded app.f is still open: the rows' record keys exist only there. It first
\ undoes the two writes to lib.f's DATA, tally.f's run of TALLY into HITS and
\ app.f's store of QUAD into HOOK, so lib.f's DATA stands as app.f found it, as it does where a
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

\ The cache path of the object stored under key.
: OBJ-PATH ( ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n :}
   0 INPUT-U !
   s" cache/hb-file-" IN+ key keyu IN+ s" .hfo" IN+
   INPUT INPUT-U @ AT ;

\ Keep the object stored under key, past its cache entry head, as file name.
: KEEP-OBJ ( ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n name:ptr nameu:n :}
   key keyu OBJ-PATH {: path:ptr pathu:n :}
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

\ name refuses because a segment of it opens or closes inside an open FFI
\ declaration, and its load wrote no object.
: DECL-OPEN ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s"  reason=" LINE$ s"  reason=ffi-declaration-open" ENDS-WITH? TTRUE
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

\ The DATA layout fixtures, kept apart from FIXTURES for the same reason as
\ FFI-FIXTURES.
: DATA-FIXTURES ( -- )
   \ dv.f's variable and created table install with their DATA bytes, the
   \ top level's store and commas among them; DV-S's string literal between
   \ them lives in the build's literal pool, not at here. df.f holds a defer
   \ alone.
   s" dv.f" S\" variable DV-V 7 DV-V !\n: DV-S ( -- ptr u8 n ) s\" lit\" ;\ncreate DV-T 3 , 4 ,\n: DV-SUM ( -- n ) DV-V @ DV-T @ + DV-T cell+ @ + ;\n" PUT
   s" df.f" S\" defer DF-D ( -- n )\n" PUT
   \ rw.f's RY rewinds DATA into RX, so RZ stands below RY although its
   \ DATA ends past both: a replay would fill backward to RZ.
   s" rw.f" S\" create RX 16 allot\ncreate RY -8 allot\ncreate RZ 8 allot\n: RW-GAP ( -- n ) RZ NULL-PTR - RX NULL-PTR - - ;\n" PUT
   \ rwc.f rewinds DATA into rwp.f's RWP-A, so rwp.f's segment after it
   \ reopens inside the one before: RWP-B lies in both segments' spans.
   s" rwp.f" S\" create RWP-A 32 allot\nrequire rwc.f\ncreate RWP-B 8 allot\n" PUT
   s" rwc.f" S\" -16 allot\n" PUT
   \ pf.f interns one body as long as a pool segment's arena, segment 0's own
   \ bytes: it opens the pool's next segment and fills it to the edge, so
   \ lp.f's literal needs the segment after that.
   s" pf.f" S\" : PF-FILL ( -- ) 0 NSTR:SOURCE-SPAN NSTR:INTERN drop ;\nPF-FILL\n" PUT
   s" lf.f" S\" : LF-S ( -- ptr u8 n ) s\" lf-pool-probe\" ;\n" PUT
   s" lp.f" S\" : LP-S ( -- ptr u8 n ) s\" lp-pool-probe\" ;\n" PUT ;

\ ffs.f declares a struct-by-value FUNCTION: against test/ffi-struct-fixture.f's
\ library, built into the tree, so its registry row carries a relocation mask
\ that a warm install declares again. FFS-S holds the s24 record {7, 3, 0, -9}.
\ Structs by value are AAPCS64, so it runs on macOS only, as in
\ src/lib/ffi-test.f. Its library is the second distinct one, which grows
\ the library table, and a file that grows a registry table refuses
\ ffi-grown: ffsl.f states the same LIBRARY line first and takes that
\ growth, so ffs.f reuses the row, as ffl2.f does ffl.f's.
: FFS-FIXTURE ( -- )
   ROOT$ STRUCT-FIXTURE:BUILD {: lib:ptr libu:n :}
   0 INPUT-U !
   S\" require src/lib/ffi-abi.f\nLIBRARY " IN+ lib libu IN+ S\" \n" IN+
   s" ffsl.f" INPUT INPUT-U @ PUT
   0 INPUT-U !
   S\" require src/lib/ffi-abi.f\nLIBRARY " IN+ lib libu IN+
   S\" \nFUNCTION: FFS-SUM24 hb_sum24 ( ptr u8 -- n ) 0 24 STRUCT-ARG ;FUNCTION\ncreate FFS-S 7 , 3 , -9 ,\n" IN+
   s" ffs.f" INPUT INPUT-U @ PUT ;

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
   \ ffo.f loads ffoc.f inside an open declaration whose clause it states:
   \ its segment closes and ffoc.f's opens with the declaration pending.
   \ ffg.f's GETENV runs inside an open declaration and leaves it unchanged.
   s" ffo.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: FFO-CRC crc32 ( n ptr u8 n -- n ) 1 2 WRITES-ARG\nrequire ffoc.f\n;FUNCTION\n" PUT
   s" ffoc.f" S\" 1 constant FFOC-K\n" PUT
   s" ffg.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: FFG-CRC crc32 ( n ptr u8 n -- n ) s\" HOME\" GETENV 2drop ;FUNCTION\n" PUT
   \ Each child crosses one boundary only: ffec.f opens with its parent's
   \ declaration pending and closes it, so only the entry check refuses it;
   \ ffxc.f opens a declaration and returns it pending, so only the exit
   \ check refuses it.
   s" ffe.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nFUNCTION: FFE-CRC crc32 ( n ptr u8 n -- n )\nrequire ffec.f\n" PUT
   s" ffec.f" S\" ;FUNCTION\n" PUT
   s" ffx.f" S\" require src/lib/ffi-abi.f\nVERSIONED-LIBRARY z 1\nrequire ffxc.f\n;FUNCTION\n" PUT
   s" ffxc.f" S\" FUNCTION: FFX-CRC crc32 ( n ptr u8 n -- n )\n" PUT
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
   s" wcb.f" S\" require src/lib/image-lifecycle.f\nIMAGE-LIFECYCLE:PREPARE\n17 constant WCB-K\n" PUT
   HB-TARGET-MACOS? if FFS-FIXTURE then ;

\ say.f's, pout.f's, fx.f's, fy.f's and capo.f's top levels print: the tee
\ records each output operation, which a hit issues again. fpar.f, fq.f,
\ fl.f and cap.f run words no trace accounts for, so they run cold on every
\ build and print what they read.
\ Kept apart from FIXTURES for its body text limit (BODYBUF-CAP).
: TEE-FIXTURES ( -- )
   s" say.f" S\" s\" said\" type cr\n66 GENIO:EMIT\n2 constant SAY-K\n" PUT
   s" pout.f" S\" s\" a\" type cr\nrequire say.f\ns\" c\" type cr\n" PUT
   s" fx.f" S\" s\" fx\" type cr\n1 constant FX-K\n" PUT
   s" fpar.f" S\" require fx.f\n: FP-CODE ( -- n ) [: s\" w\" GENIO:WRITE ;] catch ;\nFP-CODE constant FPAR-CODE\nGENIO:OUTPUT@ GENIO:FAULT@ constant FPAR-FAULT\n" PUT
   s" fy.f" S\" s\" y\" type cr\n3 constant FY-K\n" PUT
   s" fq.f" S\" require fy.f\nGENIO:OUTPUT@ GENIO:FAULT@ constant FQ-FAULT\n: FQ-W ( -- n ) [: s\" q\" GENIO:WRITE ;] catch ;\nFQ-W constant FQ-CODE\n" PUT
   s" fl.f" S\" : FL-F ( -- n ) [: GENIO:FLUSH ;] catch ;\nFL-F constant FL-CODE\nGENIO:OUTPUT@ GENIO:FAULT@ constant FL-FAULT\n: FL-W ( -- n ) [: s\" z\" GENIO:WRITE ;] catch ;\nFL-W constant FL-WCODE\n" PUT
   s" cap.f" S\" require src/lib/image-lifecycle.f\n: CAP-RUN ( -- n ) [: IMAGE-LIFECYCLE:PREPARE ;] catch ;\nCAP-RUN constant CAP-CODE\n" PUT
   s" capo.f" S\" s\" o\" type cr\n4 constant CAPO-K\n" PUT ;

\ mask.f's run of MX-RUN names data first, then runs a quotation through
\ execute: INDIRECT is admitted, and the closure walk follows the
\ quotation to its sibling mxone.f's MX-ONE, whose body the third build
\ edits; MX-ONE is not a child of mask.f, since an edited child makes
\ mask.f's replay stale and the build resumes mask.f from its source.
\ xrun.f's XGO executes the typed xt its sibling xset.f stores in XHOOK,
\ and the third build edits XA's body under the same key. dxrun.f's DXGO
\ executes the xt dxset.f stores in DXHOOK, a data cell with no colon
\ accessor whose closure would protect DXA; the third build edits DXA's
\ body. ixl.f's and
\ ixt.f's bodies run evaluate-closed on a literal: the third build edits
\ ixl.f's literal, and IX-TEN, which ixt.f's text calls. Each text stores
\ into its own file's variable: a store into an earlier file's data
\ refuses the file. The rk-*.f runs
\ push a real literal, unclassed, before or after an indirect run, and refuse
\ alike, trusted RK-TRUST too: a contract admits an unchecked body, not a
\ real literal in it, and RK-IS's real literal refuses after its `is`.
\ rk-div.f's run divides and rk-mod.f's takes a remainder: scalar ops whose
\ zero divisor traps with an outcome the operands decide, so both store.
\ db-a.f's run writes a reserved engine header cell ($3900) through
\ data-base, and db-b.f's run reads that cell, which a stored object would
\ answer stale, and refuses.
\ rebind.f's RB-B compiles `is`, which rebinds a defer still holding the
\ unset sentinel: the rebind is a slot row and the file stores.
\ uhrun.f's top level runs uhd.f's checked definer UH-DEF, and uhinit.f's
\ runs its own UH-INIT; both call the trusted, unchecked UH-ONE, whose
\ contract admits it, so both runs store, and the third build edits UH-ONE's
\ body. pnt.f runs PTR-VARIABLE and NEWTYPE at top level.
: INDIRECT-FIXTURES ( -- )
   s" mxone.f" S\" : MX-ONE ( -- n ) 1 ;\n" PUT
   s" mask.f" S\" variable MX\n: MX-RUN ( -- n ) MX @ drop [: MX-ONE ;] execute ;\nMX-RUN constant MX-K\n" PUT
   s" mxp.f" S\" require mxone.f\nrequire mask.f\n" PUT
   s" xset.f" S\" : XA ( -- n ) 1 ;\nTYPED-VARIABLE XHOOK [ -- n ]\n' XA XHOOK xt!\n" PUT
   s" xrun.f" S\" : XGO ( -- n ) XHOOK @ execute ;\nXGO constant X-K\n" PUT
   s" xpar.f" S\" require xset.f\nrequire xrun.f\n" PUT
   s" dxset.f" S\" : XT-CELL: ( -- ) create 0 , does> ( -- ptr [ -- n ] ) ;\n: DXA ( -- n ) 1 ;\nXT-CELL: DXHOOK\n' DXA DXHOOK xt!\n" PUT
   s" dxrun.f" S\" : DXGO ( -- n ) DXHOOK @ execute ;\nDXGO constant DX-K\n" PUT
   s" dxpar.f" S\" require dxset.f\nrequire dxrun.f\n" PUT
   s" uhd.f" S\" TRUSTED: UH-ONE ( -- n ) 1 ;\n: UH-DEF ( -- ) create UH-ONE , ;\n" PUT
   s" uhrun.f" S\" UH-DEF UH-X\nUH-X @ constant UH-K\n" PUT
   s" uhinit.f" S\" variable UHI-V\n: UH-INIT ( -- ) UH-ONE UHI-V ! ;\nUH-INIT\nUHI-V @ constant UHI-K\n" PUT
   s" uhp.f" S\" require uhd.f\nrequire uhrun.f\nrequire uhinit.f\n" PUT
   s" pnt.f" S\" PTR-VARIABLE PN-P\nNEWTYPE pnt\n" PUT
   s" rk-ind.f" S\" : RK-IND ( n n -- n ) [: ;] execute 1.5 fdrop + ;\n6 3 RK-IND constant RK-IND-K\n" PUT
   s" rk-base.f" S\" : RK-BASE ( n n -- n ) 1.5 fdrop + ;\n6 3 RK-BASE constant RK-BASE-K\n" PUT
   s" rk-rev.f" S\" : RK-REV ( n n -- n ) 1.5 fdrop + [: ;] execute ;\n6 3 RK-REV constant RK-REV-K\n" PUT
   s" rk-trust.f" S\" TRUSTED: RK-TRUST ( n n -- n ) 1.5 fdrop + ;\n6 3 RK-TRUST constant RK-TRUST-K\n" PUT
   s" rk-is.f" S\" defer RK-D ( -- )\n: RK-IS ( n n -- n ) [: ;] is RK-D 1.5 fdrop + ;\n6 3 RK-IS constant RK-IS-K\n" PUT
   s" rk-div.f" S\" : RK-DIV ( n n -- n ) / ;\n6 3 RK-DIV constant RK-DIV-K\n" PUT
   s" rk-mod.f" S\" : RK-MOD ( n n -- n ) mod ;\n7 3 RK-MOD constant RK-MOD-K\n" PUT
   s" db-a.f" S\" $3900 constant A-OFF\nTRUSTED: A-PUT ( n -- ) data-base A-OFF + ! ;\n7 A-PUT\n" PUT
   s" db-b.f" S\" : B-GET ( -- n ) data-base $3900 + @ ;\nB-GET constant B-K\n" PUT
   s" rebind.f" S\" defer RB-D ( -- )\n: RB-B ( -- ) [: ;] is RB-D ;\nRB-B\n" PUT
   s" ixw.f" S\" : IX-TEN ( -- n ) 10 ;\n" PUT
   s" ixl.f" S\" variable IXL-V\n: IXL-RUN ( -- ) s\" 3 IXL-V !\" evaluate-closed ;\nIXL-RUN\nIXL-V @ constant IXL-K\n" PUT
   s" ixt.f" S\" variable IXT-V\n: IXT-RUN ( -- ) s\" IX-TEN IXT-V !\" evaluate-closed ;\nIXT-RUN\nIXT-V @ constant IXT-K\n" PUT
   s" ixp.f" S\" require ixw.f\nrequire ixl.f\nrequire ixt.f\n" PUT ;

\ A run that reaches a cast stores when the cast only renames a scalar
\ representation, and refuses when its output mints a pointer. cid.f's
\ CID-ROUND casts through its package's NEWTYPE and back; rolecast.f runs the
\ seed's >LEN and LEN>N at top level. The session prints CID-K and ROLE-SEEN
\ after each load, and the warm build asserts the values its replays install.
\ mint.f's MINT-USE casts an integer to a pointer, and pb.f's PB-USE to a
\ layout whose field holds one: both runs refuse their files.
: CAST-FIXTURES ( -- )
   s" cid.f" S\" package FRAME-CID\nNEWTYPE cid\nprivate\nCAST: N>CID ( n -- cid )\nCAST: CID>N ( cid -- n )\npublic\n: CID-ROUND ( n -- n ) N>CID CID>N ;\n;package\n3 FRAME-CID:CID-ROUND constant CID-K\n" PUT
   s" rolecast.f" S\" 3 >LEN LEN>N constant ROLE-SEEN\n" PUT
   s" mint.f" S\" package FRAME-MINT\nprivate\nCAST: N>BYTES ( n -- ptr u8 )\npublic\n: MINT-USE ( n -- ptr u8 ) N>BYTES ;\n;package\n0 FRAME-MINT:MINT-USE drop\n" PUT
   s" pb.f" S\" package FRAME-PB\nprivate\nSTRUCTURE pb FIELD p ptr u8 ;STRUCTURE\nCAST: N>PB ( n -- pb )\npublic\n: PB-USE ( n -- ) N>PB drop ;\n;package\n0 FRAME-PB:PB-USE\n" PUT ;

\ cid.f's and rolecast.f's runs store; mint.f's and pb.f's refuse.
: CAST-CHECKS ( -- )
   s" a top-level run through a representation cast stores" T-LABEL
   s" cid.f" KEY$ STORED
   s" cid.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a top-level run of the seed's representation casts stores" T-LABEL
   s" rolecast.f" KEY$ STORED
   s" a run through a cast whose output mints a pointer refuses" T-LABEL
   s" mint.f" s"  reason=" LINE$ s" mint.f reason=run-host-cast word=MINT-USE" T$=
   s" mint.f" NO-OBJECT
   s" a run through a cast into a layout holding a pointer refuses" T-LABEL
   s" pb.f" s"  reason=" LINE$ s" pb.f reason=run-host-cast word=PB-USE" T$=
   s" pb.f" NO-OBJECT ;

\ sx.f loads sxm.f, then sxc.f, then compiles against sxc.f. sxm.f's top
\ level prints sx-mark and appends a byte to sxmark.txt, a run the capture
\ refuses, so it runs on every build.
: SX-FIXTURES ( -- )
   0 INPUT-U !
   S\" s\" sx-mark\" type cr\ns\" " IN+ s" sxmark.txt" AT IN+ S\" \" s\" x\" APPEND-FILE\n" IN+
   s" sxm.f" INPUT INPUT-U @ PUT
   s" sxc.f" S\" : SXC-N ( -- n ) 2 ;\n" PUT
   s" sx.f" S\" require sxm.f\nrequire sxc.f\n: SX-B ( -- n ) SXC-N 1+ ;\n" PUT ;

\ wlp.f's qualified definition makes namespace WLP with one wordlist, and
\ `package WLP` gives it a private one after WLP-W, so the two are not
\ adjacent. wlq.f's qualified definition makes namespace WLQ, and wlr.f's
\ `package WLQ` gives that earlier file's namespace its private wordlist,
\ which C is then defined into. wls.f's qualified definition makes namespace
\ WLS, it loads wlt.f, and its later segment's `package WLS` gives WLS its
\ private wordlist with no lookup into it; STALE-CON edits wlt.f to give WLS
\ that wordlist first. wlk.f requires wlx.f while its own bare wordlist WLK-W
\ is current, so wlx.f's scope names a wordlist no package record owns;
\ wlx.f reads it, defines in the global context and restores it. wlk.f
\ itself loads outside the scope it began in (scope-at-load). wlm.f makes
\ WLM-A, loads wlc.f, which makes WLC-W, then makes WLM-B: its second
\ wordlist is its item 1, apart from its child's. wld.f requires wlm.f,
\ then wmb.f, wma.f and wmc.f while WLM-B, WLM-A and WLC-W are current, so
\ each scope names a wordlist by the file that made it; like wlk.f, wld.f
\ is scope-at-load. The wlm= line is the cold/warm equality check: each
\ consumer compares the wordlist it saw with its maker's, so a warm install
\ that numbers items apart from the cold load prints a different line; this
\ suite builds no image to compare. A file names a wordlist only within one
\ top-level load, as it names a record. wln.f loads while a wordlist the
\ session made outside every file is current, which no key can name.
: WL-FIXTURES ( -- )
   s" wlp.f" S\" : WLP:A ( -- n ) 5 ;\nwordlist constant WLP-W\npackage WLP\npublic\n: B ( -- n ) 6 ;\n;package\n" PUT
   s" wlq.f" S\" : WLQ:A ( -- n ) 5 ;\n" PUT
   s" wlr.f" S\" require wlq.f\npackage WLQ\n: C ( -- n ) 1 ;\npublic\n: B ( -- n ) C 5 + ;\n;package\n" PUT
   s" wls.f" S\" : WLS:A ( -- n ) 5 ;\nrequire wlt.f\npackage WLS\n;package\n" PUT
   s" wlt.f" S\" : WLT-K ( -- n ) 1 ;\n" PUT
   s" wlk.f" S\" wordlist constant WLK-W\nget-current constant WLK-OLD\nWLK-W set-current\nrequire wlx.f\nWLK-OLD set-current\n" PUT
   s" wlx.f" S\" get-current 0 set-current constant WLX-CUR\nWLX-CUR set-current\n" PUT
   s" wld.f" S\" require wlm.f\nget-current constant WLD-OLD\nWLM-B set-current\nrequire wmb.f\nWLM-A set-current\nrequire wma.f\nWLC-W set-current\nrequire wmc.f\nWLD-OLD set-current\n" PUT
   s" wlm.f" S\" wordlist constant WLM-A\nrequire wlc.f\nwordlist constant WLM-B\n" PUT
   s" wlc.f" S\" wordlist constant WLC-W\n" PUT
   s" wma.f" S\" get-current 0 set-current constant WMA-CUR\nWMA-CUR set-current\n" PUT
   s" wmb.f" S\" get-current 0 set-current constant WMB-CUR\nWMB-CUR set-current\n" PUT
   s" wmc.f" S\" get-current 0 set-current constant WMC-CUR\nWMC-CUR set-current\n" PUT
   s" wln.f" S\" get-current 0 set-current constant WLN-CUR\nWLN-CUR set-current\n" PUT ;


\ The loader edge fixtures, kept apart from FIXTURES for the same reason as
\ FFI-FIXTURES. Every load below is a loader token's action at a loader cut,
\ whether it runs the child or finds it loaded.
: EDGE-FIXTURES ( -- )
   s" npk.f" S\" : NPK ( -- n ) 1 ;\n" PUT
   s" npc.f" S\" require npk.f\n: NPC ( -- n ) NPK 1+ ;\n" PUT
   s" nproot.f" S\" require npk.f\nrequire npc.f\n" PUT
   \ NREQ names no word of npk.f: an earlier top-level load's records hold
   \ no key in this one, which orders the file (evx.f).
   s" nreq.f" S\" s\" npk.f\" required\n: NREQ ( -- n ) 3 ;\n" PUT
   \ evreq.f's load runs inside evaluate, not at a loader cut.
   s" evreq.f" S\" s\" require npk.f\" evaluate\n" PUT
   s" nlk.f" S\" : NLK ( -- n ) 5 ;\n" PUT
   s" nlc.f" S\" require nlk.f\n: NLC ( -- n ) NLK 1+ ;\n" PUT
   s" nlroot.f" S\" require nlk.f\nrequire nlc.f\n" PUT ;

\ The raw-address fixtures. Checked code cannot put an address in undeclared
\ bytes, so raw.f, poke.f, addr.f, upd.f, off.f and own.f each load from a
\ parent (rawp.f ...) that requires the subject's dependencies checked, then
\ requires the subject inside a `0 set-check` window: the subject's requires
\ are no-op edges in its own session. Each parent itself is ordered by its
\ check@ (run-unaccounted); a window inside the subject would order the
\ subject the same way, and a dependency loaded at an earlier top level would
\ order it too (resolution-key-unknown). list.f has no dependency and loads
\ inside a window the app opens. Kept apart from FIXTURES for its body text
\ limit (BODYBUF-CAP).
: PIN-FIXTURES ( -- )
   \ raw.f's top level stores RAW-TARGET's code address into RAW-CELL's
   \ undeclared create bytes, which the object scans because the store ran
   \ unchecked; the tick is a run of the SAFE RAW-TARGET, which rawt.f
   \ defines checked.
   s" rawt.f" S\" : RAW-TARGET ( -- ) ;\n" PUT
   s" raw.f" S\" require rawt.f\ncreate RAW-CELL 0 ,\n' RAW-TARGET RAW-CELL !\n" PUT
   \ poke.f's top level stores BOARD-GO's code address 8 bytes into board.f's
   \ 16-byte BOARD, an earlier cell no address row declares, which nothing
   \ restores: the earlier-write check refuses it. board.f loads checked in poke.f's session, before the
   \ window, above its image floor.
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
   \ address of hits.f's HITS; upd.f's top level stores the address of
   \ pina.f's PIN-A into PIN-A, which refuses upd.f, since a load changes only
   \ the earlier DATA of its own session. off.f stores odeep.f's TEN with xt!,
   \ which declares the cell it writes, eight bytes into hooks.f's 16-byte
   \ PIN-HOOKS. PIN-HOOKS is a create, not a TYPED-BUFFER: that definer's
   \ accessor calls the engine's DYNAMIC-STORAGE:BOUNDS, a seed code record no
   \ REACH class covers, so a run of it is refused run-reaches-kernel-word
   \ word=BOUNDS and would order off.f's frame before the slot check. own.f's
   \ DATA holds pinb.f's PIN-B, the last record with DATA, and then
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
   s" rawp.f" S\" require rawt.f\ncheck@ 0 set-check\nrequire raw.f\nset-check\n" PUT
   s" pokep.f" S\" require board.f\ncheck@ 0 set-check\nrequire poke.f\nset-check\n" PUT
   s" addrp.f" S\" require hits.f\ncheck@ 0 set-check\nrequire addr.f\nset-check\n" PUT
   s" updp.f" S\" require pina.f\ncheck@ 0 set-check\nrequire upd.f\nset-check\n" PUT
   s" offp.f" S\" require hooks.f\nrequire odeep.f\ncheck@ 0 set-check\nrequire off.f\nset-check\n" PUT
   s" ownp.f" S\" require pinb.f\ncheck@ 0 set-check\nrequire own.f\nset-check\n" PUT ;

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
   SX-FIXTURES
   \ lbv.f carries a value across its load of lbvk.f; lbc.f loads lbca.f and
   \ lbcb.f from a colon word. Neither load is a loader token's action
   \ ending balanced at a loader cut.
   s" lbvk.f" S\" : LBV-KID ( -- n ) 2 ;\n" PUT
   s" lbv.f" S\" 1 include lbvk.f constant LBV-K\n" PUT
   s" lbca.f" S\" : LBC-A ( -- n ) 41 ;\n" PUT
   s" lbcb.f" S\" : LBC-B ( -- n ) 1 ;\n" PUT
   s" lbc.f" S\" : LBC-LOAD ( -- ) s\" lbca.f\" required s\" lbcb.f\" required ;\nLBC-LOAD\n" PUT
   \ w.f makes a wordlist no package record owns: the next one is W's next.
   s" w.f" S\" wordlist constant W\n3 constant K\n" PUT
   WL-FIXTURES
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
   \ the data word STORE itself. PEEK's contract admits a run of it, at top
   \ level or reached from SAFE-PEEK.
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
   \ exit.f's run of EX-GUARD reaches die, which ends the process: a run
   \ that dies leaves no result to store, so the closure admits it.
   s" exit.f" S\" : EX-GUARD ( n -- ) 0< if s\" neg\" 3 die then ;\n1 EX-GUARD\n1 constant EX-K\n" PUT
   s" tool-def.f" S\" : TOOL-POKE ( -- ) NCOMP:SESSION-LIVE? drop ;\n" PUT
   s" reach.f" S\" require tool-def.f\n1 constant REACH-K\nTOOL-POKE\n" PUT
   0 INPUT-U !
   S\" : FW ( -- ) s\" " IN+ s" fw.txt" AT IN+ S\" \" s\" x\" WRITE-ALL ;\nFW\n" IN+
   s" fw-def.f" INPUT INPUT-U @ PUT
   s" out.f" S\" : OUT-A ( -- ) 65 emit ;\nOUT-A\n" PUT
   INDIRECT-FIXTURES
   CAST-FIXTURES
   \ own-trust.f runs its own unchecked OT at top level, which OT's contract
   \ admits.
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
   \ The checker refuses a catch of the primitive evaluate's opaque xt, so
   \ ntp.f requires nestthrow.f inside a `0 set-check` window, after ntq.f
   \ defines the words its runs reach checked.
   s" ntq.f" S\" : NT-Q ( -- ptr u8 n ) s\" 5 throw\" ;\n: NT$ ( -- ptr u8 n ) s\" 8 allot 0 set-tier NT-Q evaluate\" ;\n" PUT
   s" nestthrow.f" S\" require ntq.f\nNT$ ' evaluate catch constant NT-RC 2drop\n1 set-tier\n" PUT
   s" ntp.f" S\" require ntq.f\ncheck@ 0 set-check\nrequire nestthrow.f\nset-check\n" PUT
   s" exec-xt.f" S\" : EX ( -- ) ;\n' EX execute\n" PUT
   s" act2.f" S\" NCOMP:SESSION-LIVE? drop\n1 constant ACT-K2\n" PUT
   s" nest-child.f" S\" 2 constant NC-K\n" PUT
   s" nest-parent.f" S\" : NP ( -- ) ;\ninclude nest-child.f\nNCOMP:SESSION-LIVE? drop\n" PUT
   s" act.f" S\" 1 constant ACT-K\nIR-CTX:SESSION-CLOSE\n" PUT
   \ watermark.f moves the user-slot watermark of the TASK the builder loaded at
   \ load time through the cell TASK hands its claimants.
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
   PIN-FIXTURES
   \ ev.f's records belong to its own top-level load. evw.f's store into EV0,
   \ evx.f's code address in EV-T and evr.f's read of EV0 reach them from
   \ later loads; ev2.f's store into its child ev3.f's EV3 refuses it, and
   \ ev4.f's store into its own EV4, in the segment after its require, does.
   s" ev.f" S\" variable EV0\ncreate EV-T 16 allot\n: EV-GO ( -- ) ;\n" PUT
   s" evw.f" S\" 9 EV0 !\n" PUT
   s" evx.f" S\" TRUSTED: EV-SET ( -- ) ['] EV-GO EV-T 8 + ! ;\nEV-SET\n" PUT
   s" evr.f" S\" EV0 @ constant EV-SEEN\n" PUT
   s" ev3.f" S\" variable EV3\n" PUT
   s" ev2.f" S\" require ev3.f\n9 EV3 !\n" PUT
   s" ev5.f" S\" variable EV5\n" PUT
   s" ev4.f" S\" variable EV4\nrequire ev5.f\n7 EV4 !\n" PUT
   \ tier.f leaves the compiler tier at 0; tierback.f restores it before
   \ its definition, since a definition the tier-0 compiler makes is a record
   \ of unknown kind.
   s" tier.f" S\" 0 set-tier\n" PUT
   \ tierpar.f's own segments leave the tier as they found it, but its key's
   \ tier is not the one its child left.
   s" tierpar.f" S\" require tier.f\n" PUT
   s" tierback.f" S\" 0 set-tier\n1 set-tier\n: TIER-T1 ( -- ) ;\n" PUT
   \ pkgc.f closes the package its loader opened. The builder made that
   \ package between loads, so its frame cannot name it and orders the file
   \ first; scope-exit would refuse it too.
   s" pkgc.f" S\" ;package\n" PUT
   \ pkgload.f requires pkgchild.f inside its package: the child's object
   \ records that scope, and the replay would load the child in the scope
   \ pkgload.f began in, so the load refuses pkgload.f.
   s" pkgload.f" S\" package FRAME-PKGLOAD\nrequire pkgchild.f\n: INSIDE ( -- n ) 3 ;\n;package\n" PUT
   s" pkgchild.f" S\" : PKG-CHILD ( -- n ) 4 ;\n" PUT
   \ newtype.f declares a type family beside a package record: the record
   \ gives it a segment to install, and the install must import the family.
   s" newtype.f" S\" package FRAME-NEWTYPE\nNEWTYPE frnt\n;package\n" PUT
   FFI-FIXTURES
   NEST-FIXTURES
   TEE-FIXTURES
   \ IMM-USER runs lib.f's immediate MARK while it compiles. app.f loads
   \ exec.f between its own definitions, and its top level stores lib.f's QUAD
   \ into lib.f's declared xt cell HOOK. tally.f runs app.f's TALLY, which
   \ adds 1 to lib.f's HITS.
   \ og.f's constants and ogw.f's colon word install on a warm build.
   s" og.f" S\" package FRAME-OG\npublic\n3 constant OG-P\n;package\n5 constant OG-G\n" PUT
   s" ogw.f" S\" : OG-W ( -- n ) 7 ;\n" PUT
   DATA-FIXTURES
   EDGE-FIXTURES
   s" jrn.f" S\" 3 constant EK-K\n" PUT
   s" pcf.f" S\" 3 constant PC-F3\npackage PCF\npublic\nexport PC-F3\n;package\n" PUT
   s" pcz.f" S\" 3 constant PC-Z3\n" PUT
   s" pcd.f" S\" 3 constant PC-D3\n" PUT
   s" pce.f" S\" 3 constant PC-K3\npackage PCE\npublic\nexport PC-K3\n;package\n" PUT
   s" app.f" APP$ PUT
   s" tally.f" S\" TALLY\n" PUT
   s" top.f" S\" require app.f\nrequire tally.f\nrequire check.f\n" PUT
   s" check.f" S\" \\ the first build has no object to check\n" PUT ;

: FRAME-CHECKS ( -- )
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   OUT$ S\" pk=7\n" CONTAINS? TTRUE
   OUT$ s" refused=1" CONTAINS? TTRUE
   OUT$ S\" con=4\n" CONTAINS? TTRUE
   OUT$ S\" dep=5\n" CONTAINS? TTRUE
   OUT$ S\" w=1\nk=3\n" CONTAINS? TTRUE
   s" rp.f's top-level require is a clean load: its object stores the load" T-LABEL
   s" rp.f" s"  loads=" s" 1" OBJECT-HAS
   s" a value carried across a load refuses its file, not the child" T-LABEL
   OUT$ S\" lbv=3\n" CONTAINS? TTRUE
   s" lbv.f" s"  reason=" LINE$ s" lbv.f reason=load-boundary" T$=
   s" lbv.f" NO-OBJECT
   s" lbvk.f" KEY$ STORED
   s" a colon word's loads refuse its file, not the children" T-LABEL
   OUT$ S\" lbc=42\n" CONTAINS? TTRUE
   \ The run trace sees the kernel loader before the child's FILE-BEGIN.
   s" lbc.f" s"  reason=" LINE$ s" lbc.f reason=run-reaches-kernel-word word=required" T$=
   s" lbc.f" NO-OBJECT
   s" lbca.f" KEY$ STORED s" lbcb.f" KEY$ STORED
   s" w.f writes its object" T-LABEL
   ERR$ s" /w.f key=" CONTAINS? TTRUE
   s" wlp.f, wlq.f and wlr.f write objects for every package wordlist shape" T-LABEL
   OUT$ S\" wlp=11\n" CONTAINS? TTRUE
   OUT$ S\" wlr=11\n" CONTAINS? TTRUE
   s" wlp.f" KEY$ STORED s" wlq.f" KEY$ STORED s" wlr.f" KEY$ STORED
   s" wls.f, which reopens its namespace after a load, writes its object" T-LABEL
   OUT$ S\" wls=6\n" CONTAINS? TTRUE
   s" wls.f" KEY$ STORED s" wlt.f" KEY$ STORED
   s" wlx.f writes its object though it loads under wlk.f's bare wordlist" T-LABEL
   OUT$ S\" wlx=-1\n" CONTAINS? TTRUE
   s" wlk.f" s"  reason=" LINE$ s" wlk.f reason=scope-at-load" T$=
   s" wlx.f" KEY$ STORED
   s" wlm.f, wlc.f and the loads under their wordlists write objects" T-LABEL
   OUT$ S\" wlm=2\n1\n-1\n-1\n-1\n" CONTAINS? TTRUE
   s" wld.f" s"  reason=" LINE$ s" wld.f reason=scope-at-load" T$=
   s" wlm.f" KEY$ STORED s" wlc.f" KEY$ STORED
   s" wma.f" KEY$ STORED s" wmb.f" KEY$ STORED s" wmc.f" KEY$ STORED
   s" wln.f, loaded under a wordlist no file made, refuses its object" T-LABEL
   OUT$ S\" wln=0\n-1\n" CONTAINS? TTRUE
   s" wln.f" FRAME$ s"  reason=scope-wordlist-unknown" ENDS-WITH? TTRUE
   s" wln.f" NO-OBJECT
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
   s" app.f consumes ONE, TWICE, TEN, SEVEN, HITS and MARK once each, SEVEN's value, MARK's run and HOOK's run and data" T-LABEL
   \ The xt! line adds 5 resolution rows (FRAME-LIB:QUAD and FRAME-LIB:HOOK 2
   \ each, namespace and public, xt! 1 global), HOOK's run and its generated
   \ body's, and HOOK's data.
   s" app.f" FRAME$ s"  ordered=0 binding=6 value=1 resolution=34 executed=3 data=1" ENDS-WITH? TTRUE
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
   s" run.f's top-level run of the trusted PEEK stores" T-LABEL
   s" run.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" run.f" FRAME$ s"  executed=" FIELD$ s" 1" T$=
   s" run.f" KEY$ STORED
   s" safe.f's run of the checked SAFE-PEEK reaches the unchecked PEEK and stores" T-LABEL
   s" safe.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" safe.f" FRAME$ s"  executed=" FIELD$ s" 1" T$=
   s" safe.f" KEY$ STORED
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
   s" lib.f's object holds its records, HITS, T and HOOK's data and its two loads" T-LABEL
   s" lib.f" s"  records=" s" 10" OBJECT-HAS
   \ DATA: HITS 8 bytes, T 16, HOOK 8, and the 14 bytes of "FRAME-LIB:MARK"
   \ that its interpreted s" copies to HERE.
   s" lib.f" s"  data=" s" 46" OBJECT-HAS
   s" lib.f" s"  loads=" s" 2" OBJECT-HAS
   s" lib.f" s"  segments=" s" 1" OBJECT-HAS
   s" app.f's object holds six loads, the last between its own definitions" T-LABEL
   s" app.f" s"  records=" s" 4" OBJECT-HAS
   s" app.f" s"  loads=" s" 6" OBJECT-HAS
   s" app.f" s"  segments=" s" 3" OBJECT-HAS
   s" base.f" s"  loads=" s" 0" OBJECT-HAS
   s" base.f" s"  segments=" s" 1" OBJECT-HAS
   s" each object holds one checker slice per load and one for itself" T-LABEL
   s" base.f" s"  checks=" s" 1" OBJECT-HAS
   s" lib.f" s"  checks=" s" 3" OBJECT-HAS
   s" app.f" s"  checks=" s" 7" OBJECT-HAS
   s" lib.f's declared xt cell HOOK is its one slot" T-LABEL
   s" lib.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f's store into lib.f's HOOK is its one slot, binding HOOK and QUAD beyond its frame's rows" T-LABEL
   s" app.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" app.f" FRAME$ s"  rows=" FIELD$ FIELD-N 2 + T=
   s" tally.f's run of TALLY changes lib.f's HITS, which nothing restores, and refuses naming HITS" T-LABEL
   s" tally.f" s"  reason=" LINE$ s" tally.f reason=earlier-write word=HITS" T$=
   s" tally.f" NO-OBJECT
   s" raw.f's undeclared code address refuses its object, naming the cell" T-LABEL
   s" raw.f" s"  reason=" LINE$
      s" raw.f reason=raw-cell owner=RAW-CELL offset=0 value=0x" STARTS-WITH? TTRUE
   ERR$ s" raw.f key=" CONTAINS? TFALSE
   s" poke.f's code address in earlier DATA refuses its object, naming the cell" T-LABEL
   s" poke.f" s"  reason=" LINE$
      s" poke.f reason=earlier-write word=BOARD" T$=
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
   s" upd.f's store of PIN-A's address into PIN-A refuses its object, naming PIN-A" T-LABEL
   s" upd.f" s"  reason=" LINE$ s" upd.f reason=earlier-write word=PIN-A" T$=
   s" upd.f" NO-OBJECT
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
   s" a file loaded inside a package stores" T-LABEL
   s" pkgchild.f" DEFS$ s" 1" T$=
   s" pkgchild.f" KEY$ STORED
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
   s" a store into a child's variable in the same load refuses, naming it" T-LABEL
   s" ev2.f" s"  reason=" LINE$ s" ev2.f reason=earlier-write word=EV3" T$=
   s" ev2.f" NO-OBJECT
   s" a later segment's store into the file's own earlier segment refuses, naming it" T-LABEL
   s" ev4.f" s"  reason=" LINE$ s" ev4.f reason=earlier-write word=EV4" T$=
   s" ev4.f" NO-OBJECT
   s" a file that closes its loader's package has no object" T-LABEL
   s" pkgc.f" FRAME$ s"  reason=scope-key-unknown word=FRAME-P3" ENDS-WITH? TTRUE
   s" pkgc.f" s"  reason=" LINE$ s" pkgc.f reason=ordered-dependency" T$=
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
   HB-TARGET-MACOS? if
      s" a LIBRARY line that grows the library table refuses ffi-grown" T-LABEL
      s" ffsl.f" s"  reason=" LINE$ s" ffsl.f reason=ffi-grown" T$=
      s" ffsl.f" NO-OBJECT
      s" a struct-by-value FUNCTION: file stores its object" T-LABEL
      OUT$ S\" ffs=1134932\n" CONTAINS? TTRUE
      s" ffs.f" KEY$ STORED
   then
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
   s" wcb.f" s"  reason=run-unaccounted word=PREPARE" RUN-REFUSED
   s" a FUNCTION: clause run is admitted; a load inside the declaration refuses both files" T-LABEL
   s" ffo.f" DECL-OPEN
   s" ffoc.f" DECL-OPEN
   s" a child that closes its parent's declaration refuses on entry" T-LABEL
   s" ffec.f" DECL-OPEN
   s" a child that returns a declaration pending refuses on exit" T-LABEL
   s" ffxc.f" DECL-OPEN
   s" a run inside a declaration that leaves it unchanged refuses" T-LABEL
   s" ffg.f" s"  reason=run-unaccounted word=GETENV" RUN-REFUSED ;

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
   s" exit.f" NO-RUN-REASON
   s" off.f" NO-RUN-REASON
   s" hooks.f" NO-RUN-REASON
   s" raw.f" NO-RUN-REASON
   s" poke.f" NO-RUN-REASON
   s" mask.f" NO-RUN-REASON
   s" xrun.f" NO-RUN-REASON
   s" dxrun.f" NO-RUN-REASON
   s" ixl.f" NO-RUN-REASON
   s" ixt.f" NO-RUN-REASON
   s" ffo.f" NO-RUN-REASON ;

\ What a top-level run reaches: a checked definition and a classed
\ primitive are admitted; a seed word no REACH class covers refuses.
: RUN-CHECKS ( -- )
   s" cread.f runs the checked CPEEK, which reads CSTORE: one run, one data read" T-LABEL
   s" cread.f" RAN
   s" act-ok.f's run reaches the store guard (PROT-SPAN), and it stores" T-LABEL
   s" act-ok.f" KEY$ STORED
   s" act-ok.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" exit.f's run reaches die, which ends the process, and it stores" T-LABEL
   s" exit.f" KEY$ STORED
   s" exit.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
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
   s" a top level that prints through a non-terminal device stores its output" T-LABEL
   s" say.f" KEY$ STORED
   s" say.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" pout.f" KEY$ STORED
   s" fx.f" KEY$ STORED
   s" fpar.f" NO-OBJECT
   s" a file opened over a faulted device stores its output" T-LABEL
   s" fy.f" KEY$ STORED
   s" capo.f" KEY$ STORED
   s" cap.f" NO-OBJECT
   s" the device sees each operation, fault and flush error as it would without the tee" T-LABEL
   TEE$ TEE-COLD T$=
   s" a forward keeps the fault its device latched" T-LABEL
   OUT$ S\" fwd=0\n5\n0\n" CONTAINS? TTRUE
   s" a run whose body names data and then executes a quotation stores" T-LABEL
   s" mask.f" KEY$ STORED
   s" mask.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   OUT$ S\" mx=1\n" CONTAINS? TTRUE
   s" a run that executes an earlier file's typed xt cell stores" T-LABEL
   s" xrun.f" KEY$ STORED
   s" xrun.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   OUT$ S\" xk=1\n" CONTAINS? TTRUE
   s" a run that executes an earlier file's data xt cell stores" T-LABEL
   s" dxrun.f" KEY$ STORED
   s" dxrun.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   OUT$ S\" dxk=1\n" CONTAINS? TTRUE
   s" a run of a checked definer that calls an unchecked word stores" T-LABEL
   s" uhrun.f" KEY$ STORED
   s" uhrun.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" uhrun.f" FRAME$ s"  executed=" FIELD$ s" 1" T$=
   OUT$ S\" uh=1\n" CONTAINS? TTRUE
   s" an own word's run that calls an earlier file's trusted word stores" T-LABEL
   s" uhinit.f" KEY$ STORED
   s" uhinit.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   OUT$ S\" uhi=1\n" CONTAINS? TTRUE
   s" runs whose bodies evaluate a closed literal store" T-LABEL
   s" ixl.f" KEY$ STORED
   s" ixl.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" ixt.f" KEY$ STORED
   s" ixt.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   OUT$ S\" ixl=3\n" CONTAINS? TTRUE
   OUT$ S\" ixt=10\n" CONTAINS? TTRUE
   s" a top-level run of the file's own trusted word stores" T-LABEL
   s" own-trust.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" own-trust.f" KEY$ STORED
   s" a real literal refuses its file before or after an indirect run" T-LABEL
   s" rk-ind.f" s"  reason=" LINE$ s" rk-ind.f reason=run-host-unknown word=RK-IND" T$=
   s" rk-base.f" s"  reason=" LINE$ s" rk-base.f reason=run-host-unknown word=RK-BASE" T$=
   s" rk-rev.f" s"  reason=" LINE$ s" rk-rev.f reason=run-host-unknown word=RK-REV" T$=
   s" a trusted word's contract does not admit its real literal" T-LABEL
   s" rk-trust.f" s"  reason=" LINE$ s" rk-trust.f reason=run-host-unknown word=RK-TRUST" T$=
   s" a compiled is does not admit a later real literal" T-LABEL
   s" rk-is.f" s"  reason=" LINE$ s" rk-is.f reason=run-host-unknown word=RK-IS" T$=
   s" rk-ind.f" NO-OBJECT
   s" a top-level run of a division stores" T-LABEL
   s" rk-div.f" KEY$ STORED
   s" rk-div.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a top-level run of a remainder stores" T-LABEL
   s" rk-mod.f" KEY$ STORED
   s" rk-mod.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" a run reading a data-base cell an earlier file's run wrote refuses" T-LABEL
   s" db-b.f" s"  reason=" LINE$ s" db-b.f reason=run-reaches-kernel-word word=data-base" T$=
   s" db-b.f" NO-OBJECT
   s" a compiled is on an unset defer stores its file" T-LABEL
   s" rebind.f" KEY$ STORED
   s" rebind.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
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
   s" a warm build issues each recorded output operation as the cold one did" T-LABEL
   s" pout.f" REUSED s" say.f" REUSED s" fx.f" REUSED s" fy.f" REUSED
   s" capo.f" REUSED
   TEE$ TEE-WARM T$=

   s" a warm build installs newtype.f with the type family it declares" T-LABEL

   s" newtype.f" REUSED
   s" con-dep.f, which requires con.f, replays its object over con.f's installed records" T-LABEL
   OUT$ S\" dep=5\n" CONTAINS? TTRUE
   s" con-dep.f" REUSED
   s" a warm build replays rp.f, which requires rc.f between its definitions" T-LABEL
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   s" rc.f" REUSED s" rp.f" REUSED
   s" a warm build installs w.f, whose wordlist no package record owns, as the cold load made it" T-LABEL
   OUT$ S\" w=1\nk=3\n" CONTAINS? TTRUE
   s" w.f" REUSED
   s" a warm build replays cid.f's and rolecast.f's representation casts with their values" T-LABEL
   OUT$ S\" cid=3\n" CONTAINS? TTRUE
   OUT$ S\" role=3\n" CONTAINS? TTRUE
   s" cid.f" REUSED s" rolecast.f" REUSED
   s" a warm build installs wlp.f, whose package takes its private wordlist after another wordlist" T-LABEL
   OUT$ S\" wlp=11\n" CONTAINS? TTRUE
   s" wlp.f" REUSED
   s" a warm build installs wlr.f, which gives an earlier file's namespace its private wordlist" T-LABEL
   OUT$ S\" wlr=11\n" CONTAINS? TTRUE
   s" wlq.f" REUSED s" wlr.f" REUSED
   s" a warm build installs wls.f, whose later segment gives its namespace its private wordlist" T-LABEL
   OUT$ S\" wls=6\n" CONTAINS? TTRUE
   s" wls.f" REUSED s" wlt.f" REUSED
   s" a warm build installs wlx.f under wlk.f's bare wordlist" T-LABEL
   OUT$ S\" wlx=-1\n" CONTAINS? TTRUE
   s" wlk.f" s"  reason=" LINE$ s" wlk.f reason=scope-at-load" T$=
   s" wlx.f" REUSED
   s" a warm build installs wlm.f's wordlists apart from wlc.f's, as the cold load made them" T-LABEL
   OUT$ S\" wlm=2\n1\n-1\n-1\n-1\n" CONTAINS? TTRUE
   s" wld.f" s"  reason=" LINE$ s" wld.f reason=scope-at-load" T$=
   s" wlm.f" REUSED s" wlc.f" REUSED
   s" wma.f" REUSED s" wmb.f" REUSED s" wmc.f" REUSED
   s" a warm build compiles wln.f again under another wordlist no file made" T-LABEL
   OUT$ S\" wln=-1\n0\n" CONTAINS? TTRUE
   s" wln.f" FRAME$ s"  reason=scope-wordlist-unknown" ENDS-WITH? TTRUE
   s" wln.f" NO-OBJECT
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
   s" a warm build installs df.f's defer record" T-LABEL
   s" df.f" REUSED
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
   \ The frame suite builds no image, so no product bytes compare: the cold
   \ build's ffs= line printed again is the cold == warm check.
   HB-TARGET-MACOS? if
      s" a warm build installs ffs.f's row with its relocation mask" T-LABEL
      OUT$ S\" ffs=1134932\n" CONTAINS? TTRUE
      s" ffs.f" REUSED
   then
   s" a warm build refuses each refused FFI file again by its run reason" T-LABEL
   s" ffn.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" sel.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" selb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" scopeb.f" s"  reason=run-unaccounted word=PROCESS-SYMBOLS" RUN-REFUSED
   s" wrb.f" s"  reason=run-unaccounted word=true" RUN-REFUSED
   s" wcb.f" s"  reason=run-unaccounted word=PREPARE" RUN-REFUSED
   s" a warm build refuses a load inside a declaration, and a run that leaves it unchanged, again" T-LABEL
   s" ffo.f" DECL-OPEN
   s" ffoc.f" DECL-OPEN
   s" ffec.f" DECL-OPEN
   s" ffxc.f" DECL-OPEN
   s" ffg.f" s"  reason=run-unaccounted word=GETENV" RUN-REFUSED
   s" a warm build runs act.f again, which closes the compiler session" T-LABEL
   s" act.f" NO-OBJECT
   OUT$ S\" session=0\n" CONTAINS? TTRUE
   s" a warm build names no run reason for a file whose runs are admitted" T-LABEL
   RUNS-ADMITTED ;

\ name's replay went stale at the segment row seg names, with the reason its
\ stale line gives after the number, and the build went
\ on in the same process from name's source where the load before that row
\ returned: name is refused as segment-stale and writes no object.
: RESUMED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n seg:ptr segu:n :}
   0 INPUT-U !
   s" native-file: stale path=" IN+ name nameu AT IN+ s"  segment=" IN+
   seg segu IN+ S\" \n" IN+
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   name nameu s"  reason=" LINE$ {: line:ptr lineu:n :}
   0 INPUT-U !
   name nameu IN+ s"  reason=segment-stale" IN+
   line lineu INPUT INPUT-U @ T$=
   name nameu NO-OBJECT ;

\ A changed constant leaves con.f's object supported but stale: the warm build
\ checks it, compiles con.f again and rewrites the object, kept as con-warm.obj.
\ con-dep.f's replay, past con.f's load at row 0, then finds row 1 stale,
\ so the build compiles the rest of con-dep.f from source and discards its
\ object. The next build compiles con-dep.f alone and stores it. An edited
\ wlt.f gives WLS its private wordlist, so wls.f's second segment, which
\ would give WLS one, is stale the same way.
: STALE-CON ( -- )
   s" con.f" S\" 4 constant CON-K\nCON-K 1+ constant CON-USE\n" PUT
   s" wlt.f" S\" package WLS\n: K ( -- n ) 7 ;\n;package\n: WLT-K ( -- n ) 2 ;\n" PUT
   s" a warm build compiles a stale con.f again and resumes con-dep.f" T-LABEL
   PROBE
   s" probe-stale-out.log" OUT$ PUT
   s" probe-stale-err.log" ERR$ PUT
   SUCCESS
   OUT$ S\" con=5\n" CONTAINS? TTRUE
   OUT$ S\" dep=6\n" CONTAINS? TTRUE
   s" con-dep.f" s" 1" RESUMED
   s" a warm build finds wls.f's reopen stale after the edited wlt.f gave WLS its private wordlist" T-LABEL
   OUT$ S\" wls=7\n" CONTAINS? TTRUE
   s" wls.f" s" 3 reason=wordlist-rows" RESUMED
   ERR$ s" /con.f records=" CONTAINS? TFALSE
   s" con.f" KEY$ s" con-warm.obj" KEEP-OBJ
   s" the next build compiles only con-dep.f and wls.f and stores them" T-LABEL
   PROBE
   s" probe-next-out.log" OUT$ PUT
   s" probe-next-err.log" ERR$ PUT
   SUCCESS
   OUT$ S\" dep=6\n" CONTAINS? TTRUE
   s" con.f" REUSED
   s" con-dep.f" KEY$ STORED
   OUT$ S\" wls=7\n" CONTAINS? TTRUE
   s" wlt.f" REUSED
   s" wls.f" KEY$ STORED ;

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

\ Each loader action is an edge in its file's object, whether the child ran
\ or was found loaded.
: EDGE-CHECKS ( -- )
   s" a load of a file already loaded is an edge in the loading file's object" T-LABEL
   OUT$ S\" npc=2\n" CONTAINS? TTRUE
   s" npc.f" s"  loads=" s" 1" OBJECT-HAS
   s" a known required is an edge, and its file stores its object" T-LABEL
   OUT$ S\" nreq=3\n" CONTAINS? TTRUE
   s" nreq.f" s"  loads=" s" 1" OBJECT-HAS
   s" nreq.f" KEY$ STORED
   s" a load inside an evaluated text refuses its file" T-LABEL
   s" evreq.f" s"  reason=" LINE$ s" evreq.f reason=load-boundary" T$=
   s" evreq.f" NO-OBJECT ;

: EDGE-WARM ( -- )
   s" a warm build installs the objects of files whose edges found their child loaded" T-LABEL
   s" npc.f" REUSED s" nreq.f" REUSED s" npk.f" REUSED
   OUT$ S\" npc=2\n" CONTAINS? TTRUE
   OUT$ S\" nreq=3\n" CONTAINS? TTRUE ;

\ The third build's edits: nproot.f no longer loads npk.f before npc.f, and
\ nlk.f gains a comment.
: EDGE-EDITS ( -- )
   s" nproot.f" S\" require npc.f\n" PUT
   s" nlk.f" S\" \\ edited\n: NLK ( -- n ) 5 ;\n" PUT ;

: EDGE-REBUILT ( -- )
   s" a removed earlier provider compiles the child and its own child again" T-LABEL
   OUT$ S\" npc=2\n" CONTAINS? TTRUE
   ERR$ s" /npc.f records=" CONTAINS? TFALSE
   s" npk.f" KEY$ STORED
   s" a comment in a leaf compiles only the leaf" T-LABEL
   s" nlroot.f" REUSED s" nlc.f" REUSED
   ERR$ s" /nlk.f records=" CONTAINS? TFALSE
   s" nlk.f" KEY$ STORED ;

\ RC-N's new effect leaves rp.f's first segment fresh and its second stale:
\ the replay has installed RP-A and loaded rc.f, so the build names the
\ event row it stopped at, row 3 after RP-A's segment and history rows and
\ rc.f's load, and compiles the rest of rp.f from source. SXC-N's new effect does the same to sx.f past
\ its loads of sxm.f and sxc.f at rows 0 and 1, so it resumes at row 2; the
\ resume never loads sxm.f again, so its marker byte and its line come once.
\ npc.f's load of npk.f is row 0, so an effect edit of npk.f resumes it at
\ row 1.
: RC-INTERFACE ( -- )
   s" rc.f" S\" : RC-N ( -- u8 ) 2 ;\n" PUT
   s" sxc.f" S\" : SXC-N ( -- u8 ) 2 ;\n" PUT
   s" npk.f" S\" : NPK ( -- u8 ) 1 ;\n" PUT
   s" nproot.f" S\" require npk.f\nrequire npc.f\n" PUT
   s" sxmark.txt" AT FILE-SIZE {: marks:n :}
   s" an effect edit of rc.f resumes rp.f at its stale second segment" T-LABEL
   PROBE
   s" rc-interface-out.log" OUT$ PUT
   s" rc-interface-err.log" ERR$ PUT
   SUCCESS
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   s" rp.f" s" 3" RESUMED
   s" a resumed sx.f runs its cold child sxm.f once" T-LABEL
   OUT$ S\" sx=3\n" CONTAINS? TTRUE
   s" sx.f" s" 2" RESUMED
   s" sxmark.txt" AT FILE-SIZE marks 1+ T=
   s" an effect edit of npk.f resumes npc.f after its edge" T-LABEL
   OUT$ S\" npc=2\n" CONTAINS? TTRUE
   s" npc.f" s" 1" RESUMED ;

\ Keep line in a LINE-CAP buffer and its length in a cell.
: KEEP-LINE ( ptr u8 n ptr u8 ptr n -- )
   {: line:ptr lineu:n buf:ptr bufu:ptr :}
   lineu LINE-CAP > if T-FAIL exit then
   line buf lineu BYTE-COPY lineu bufu ! ;

public
\ The warm build reuses the files whose runs execute an xt or a closed text.
: INDIRECT-WARM ( -- )
   s" a warm build installs the objects of files whose runs execute an xt or a closed text" T-LABEL
   s" mask.f" REUSED s" xrun.f" REUSED s" dxrun.f" REUSED
   s" ixl.f" REUSED s" ixt.f" REUSED s" uhrun.f" REUSED s" uhinit.f" REUSED
   s" run.f" REUSED s" pnt.f" REUSED
   OUT$ S\" mx=1\n" CONTAINS? TTRUE
   OUT$ S\" uh=1\n" CONTAINS? TTRUE
   OUT$ S\" uhi=1\n" CONTAINS? TTRUE
   OUT$ S\" xk=1\n" CONTAINS? TTRUE
   OUT$ S\" dxk=1\n" CONTAINS? TTRUE
   OUT$ S\" ixl=3\n" CONTAINS? TTRUE
   OUT$ S\" ixt=10\n" CONTAINS? TTRUE ;

\ The third build's edits: MX-ONE's, XA's, DXA's and UH-ONE's bodies,
\ IX-TEN's body and ixl.f's literal.
: INDIRECT-EDITS ( -- )
   s" mxone.f" S\" : MX-ONE ( -- n ) 2 ;\n" PUT
   s" xset.f" S\" : XA ( -- n ) 5 ;\nTYPED-VARIABLE XHOOK [ -- n ]\n' XA XHOOK xt!\n" PUT
   s" dxset.f" S\" : XT-CELL: ( -- ) create 0 , does> ( -- ptr [ -- n ] ) ;\n: DXA ( -- n ) 5 ;\nXT-CELL: DXHOOK\n' DXA DXHOOK xt!\n" PUT
   s" ixw.f" S\" : IX-TEN ( -- n ) 20 ;\n" PUT
   s" uhd.f" S\" TRUSTED: UH-ONE ( -- n ) 2 ;\n: UH-DEF ( -- ) create UH-ONE , ;\n" PUT
   s" ixl.f" S\" variable IXL-V\n: IXL-RUN ( -- ) s\" 4 IXL-V !\" evaluate-closed ;\nIXL-RUN\nIXL-V @ constant IXL-K\n" PUT ;

\ Each edit misses the file whose run reached it, and the rerun's values are
\ those a cold build of the edited sources computes.
: INDIRECT-REBUILT ( -- )
   s" an edit of the body a run executes compiles the running file again" T-LABEL
   ERR$ s" /mask.f records=" CONTAINS? TFALSE
   OUT$ S\" mx=2\n" CONTAINS? TTRUE
   ERR$ s" /xrun.f records=" CONTAINS? TFALSE
   OUT$ S\" xk=5\n" CONTAINS? TTRUE
   ERR$ s" /dxrun.f records=" CONTAINS? TFALSE
   OUT$ S\" dxk=5\n" CONTAINS? TTRUE
   ERR$ s" /uhrun.f records=" CONTAINS? TFALSE
   OUT$ S\" uh=2\n" CONTAINS? TTRUE
   ERR$ s" /uhinit.f records=" CONTAINS? TFALSE
   OUT$ S\" uhi=2\n" CONTAINS? TTRUE
   s" an edit of a closed literal or of a word its text runs compiles the file again" T-LABEL
   ERR$ s" /ixl.f records=" CONTAINS? TFALSE
   ERR$ s" /ixt.f records=" CONTAINS? TFALSE
   OUT$ S\" ixl=4\n" CONTAINS? TTRUE
   OUT$ S\" ixt=20\n" CONTAINS? TTRUE ;

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
   CAST-CHECKS
   EDGE-CHECKS
   s" lib.f" KEY$ STORED
   s" a cold build runs dv.f and stores its object" T-LABEL
   OUT$ S\" dv=14\n" CONTAINS? TTRUE
   OUT$ S\" dvs=lit\n" CONTAINS? TTRUE
   s" dv.f" KEY$ STORED
   s" a cold build runs rw.f, whose DATA rewinds between creates, and stores no object" T-LABEL
   OUT$ S\" rw=8\n" CONTAINS? TTRUE
   s" rw.f" s"  reason=" LINE$ s" rw.f reason=data-rewound" T$=
   s" rw.f" NO-OBJECT
   s" a cold build runs rwp.f, whose segment after rwc.f reopens inside the one before, and stores no object" T-LABEL
   OUT$ S\" rwp=16\n" CONTAINS? TTRUE
   s" rwp.f" s"  reason=" LINE$ s" rwp.f reason=data-rewound" T$=
   s" rwp.f" NO-OBJECT
   s" a cold build with room in the literal pool stores lf.f's object" T-LABEL
   OUT$ S\" lf=lf-pool-probe\n" CONTAINS? TTRUE
   s" lf.f" KEY$ STORED
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
   \ app.f's object would install; without it this process compiles app.f
   \ again, and the line compares the two processes' exports.
   APP-KEY KEY-LEN OBJ-PATH REMOVE-FILE
   1 FILL-POOL !
   PROBE
   0 FILL-POOL !
   SUCCESS WARM-CHECKS
   EDGE-WARM
   s" app.f" OBJECT$ APP-LINE APP-LINE-U @ T$=
   s" a warm build compiles rw.f again and installs lf.f's object" T-LABEL
   OUT$ S\" rw=8\n" CONTAINS? TTRUE
   s" rw.f" NO-OBJECT
   OUT$ S\" lf=lf-pool-probe\n" CONTAINS? TTRUE
   s" lf.f" REUSED
   INDIRECT-WARM
   \ pf.f fills the literal pool's write segment, so lp.f's literal needs a
   \ new one: lp.f's object, stored with room, misses before it installs
   \ anything, and its compile opens that segment inside lp.f, so it stores
   \ no object.
   s" a warm build misses lp.f's object where the literal pool has no room for its literal" T-LABEL
   0 INPUT-U !
   s" native-file: miss path=" IN+ s" lp.f" AT IN+ s"  reason=literal-pool" IN+
   ERR$ INPUT INPUT-U @ CONTAINS? TTRUE
   OUT$ S\" lp=lp-pool-probe\n" CONTAINS? TTRUE
   s" lp.f" NO-OBJECT
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
   INDIRECT-EDITS
   EDGE-EDITS
   s" comments in lib.f keep lib.f's key, and app.f's object installs" T-LABEL
   PROBE SUCCESS
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   s" lib.f" DEFS$ s" 5" T$=
   s" lib.f" KEY$ LIB-KEY KEY-LEN T$=
   s" app.f" REUSED
   INDIRECT-REBUILT
   EDGE-REBUILT
   s" a comment compiles hch.f between installs of hg1.f and hg2.f inside the installing hpar.f" T-LABEL
   OUT$ S\" hp=1\n1\n2\n" CONTAINS? TTRUE
   ERR$ s" /hch.f records=" CONTAINS? TFALSE
   s" hpar.f" REUSED s" hg1.f" REUSED s" hg2.f" REUSED
   s" a body edit of rc.f compiles rc.f and replays rp.f around it" T-LABEL
   OUT$ S\" rp=3\n" CONTAINS? TTRUE
   ERR$ s" /rc.f records=" CONTAINS? TFALSE
   s" rp.f" REUSED
   s" a byte allotted before dv.f still installs its object" T-LABEL
   s" dv.f" REUSED
   OUT$ S\" dv=14\n" CONTAINS? TTRUE
   s" an earlier file that now makes FRAME-PK refuses pk.f's object: PK-SEVEN binds its private word" T-LABEL
   OUT$ S\" pk=70\n" CONTAINS? TTRUE
   ERR$ s" /pk.f records=" CONTAINS? TFALSE
   ERR$ s" dependency miss: namespace shape changed" CONTAINS? TTRUE
   s" a changed selection resumes the file that consumed it after its require" T-LABEL
   s" selc.f" s" 1 reason=ffi-rows" RESUMED
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
   \ RC-INTERFACE runs the whole probe, which LOADER-FIRST's ret-late.f
   \ would refuse.
   RC-INTERFACE
   LOADER-FIRST
   T-REPORT
   s" file frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-FRAME-TEST:MAIN
