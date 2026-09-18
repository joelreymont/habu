\ layout.f - shared native image, dictionary, and snapshot layout constants.
\ The cold prefix loads stack-abi.f first; require is not defined at this stage.

20 constant XREG-RBASE
26 constant DBASE
27 constant NDICT
28 constant CP

\ ---- the general registers the running engine occupies -----------------------
\ THE authority. A compiled routine that writes one of these destroys the engine
\ underneath itself: a probe that allocated x20 as a scratch base replaced the
\ DATA/RBASE value and the process died 134 (CG-13). The compiler's own machine
\ contract (src/compiler/native-effect.f) derives its GPR set by REMOVING this mask,
\ so a register claimed here is refused by every GPR constructor, set, sequence,
\ pool and allocation path in the chain without a second edit anywhere.
\
\ It is built from the register constants rather than written out as a list, so
\ claiming a new engine register is one line here and nothing else.
\
\ ENGINE-GPR:DSTACK is declared HERE and not read from src/arch/arm64/mnem.f's
\ XDS, even though mnem.f is that number's historic home. mnem.f is an
\ emitter-side vocabulary only the BUILD chain compiles; this file is loaded by
\ both build chains and belongs to every engine's own prefix, and mnem.f does
\ not. A derivation through XDS therefore compiles during the build and dies
\ E-UNDEFINED at runtime - which is exactly what happened when it was tried.
\ The two must agree, and src/habu/rt.f - the first consumer of XDS, loaded
\ after this file in both build chains - executes that agreement
\ (RT:DSTACK-AGREE) and dies on mismatch, so a change to either stops the build
\ rather than drifting.
package ENGINE-GPR

public

19 constant DSTACK

1 DSTACK lshift
1 XREG-RBASE lshift or
1 DBASE lshift or
1 NDICT lshift or
1 CP lshift or
constant MASK

;package

\ Fixed budget for dictionary and code, including the retained build host and
\ its replacement core. The bounds checks and boot BL-range assertion remain
\ mandatory. Keep the Gforth recovery mirror in bootstrap/cg/forth.fs in step.
$2000000 constant REGION
\ RBASE-VA: snapshot CANONICAL region base (a fixed portability sentinel, NOT the
\ runtime map address). The live JIT region maps at __text base + REGION-OFF (see
\ habu2.f EM-MMAP-CODE-REGION) so every call site and callee sit inside BL's
\ +/-128 MiB reach; snap-rebase canonicalizes region-internal pointers to this
\ sentinel and the loader rebases them back to the live region base, keeping images
\ byte-identical across runs and imgdump's region->offset math target-independent.
$300000000 constant RBASE-VA
\ Canonical code-region hint past __text. The engine clamps it after the loaded
\ image and checks the resulting mapping against BL-REACH at boot.
$1000000 constant REGION-OFF
\ BL-REACH: AArch64 BL imm26 reach (+/-128 MiB). The boot assertion dies BL-RANGE-RC
\ if the mapped region falls outside this of __text -- the permanent guarantee stage
\ C's direct-BL emission relies on.
$8000000 constant BL-REACH
81 constant BL-RANGE-RC
$48425350414E5321 constant SNAP-MAGIC
\ Version 4: the live JIT region moved from a fixed VA to __text base + REGION-OFF
\ (BL range); region-internal pointers now canonicalize to the RBASE-VA sentinel and
\ the loader rebases them to the live region base. A pre-4 engine maps the region at
\ the old fixed VA and cannot relocate a v4 image's region pointers, so it fails
\ closed rc 80.
\ Version 5: the region no longer has to land at a particular address at all. The
\ loader accepts whatever base the kernel gives it and relocates every recorded
\ region-to-text call displacement, and every persisted data cell that was
\ declared to hold a region address (both tables live in the SNAP-RELOC band near
\ the end of this file). A version 5 image therefore stores its call immediates in
\ the canonical "region sits exactly REGION-OFF above __text" form, and its
\ declared address cells relative to the RBASE-VA sentinel, rather than in the
\ writing run's own form. A version 4 engine would read both as live values and
\ jump to wild addresses, so it must fail closed rc 80 instead.
\ Version 6: the address literals the compiler builds inside region code -- the
\ four-instruction MOVZ/MOVK chain that pushes a quotation entry or a `[']`
\ target -- are canonicalized as well, region-valued ones against
\ the RBASE-VA sentinel and engine-text-valued ones against text base 0, from a
\ third table in the SNAP-RELOC band. A version 5 image stores those chains as
\ the writing run's own absolute addresses, so a version 5 engine and a version 6
\ image disagree about what the chain bytes mean in both directions and each must
\ fail closed rc 80 rather than execute the other's literals.
\ Version 7 tags each row of the existing address-cell table as either an XT or
\ a DATA pointer. The snapshot pass relocates only XT values; AOT capture uses
\ the same declaration to rebase self-window DATA pointers. A version 6 engine
\ would interpret the tag as part of an offset, so the hard version equality is
\ required in both directions.
\ Version 8 adds the persisted application startup execution token.
\ Version 9 stores the address-cell vector through a checked DATA-relative header.
9 constant SNAP-FORMAT-VERSION

\ --- snapshot trailer geometry: the single owner ----------------------------
\ The trailer is the last thing in the authenticated text extent, so its base is
\ (the header's text-size field + IMAGE-TEXT-TRAILER-ADJ) - SNAP-TRL-BYTES; the
\ codesign blob and any page padding follow it and are not part of the extent.
\ Everything that reads or writes a trailer derives its size and field offsets
\ from here: the writer (src/habu/snap-lib.f SNAP:WRITE-BYTES), the loader
\ (src/habu/habu2.f EM-SNAPSHOT-RESTORE), the image dumper (tools/imgdump.f) and
\ the two fixtures that doctor a real image (test/snapshot-writer.f,
\ tools/build-fixpoint-test.f). They diverged once - the readers kept the legacy
\ 40-byte size after the format grew to 48 - and every reader then addressed the
\ wrong cells while still finding plausible values, so the size lives in exactly
\ one place now.
48 constant SNAP-TRL-BYTES        \ magic, text base, ndict, region len, data len, version
8 constant SNAP-TRL-TBASE         \ snapshot-time text base (canonically 0)
16 constant SNAP-TRL-NDICT        \ dictionary record count
24 constant SNAP-TRL-REGLEN       \ region payload length
32 constant SNAP-TRL-DATALEN      \ data payload length
40 constant SNAP-TRL-VERSION      \ SNAP-FORMAT-VERSION of the writing engine
\ The pre-version trailer. Only the loader may name it, and only to recognise a
\ legacy image and fail it closed rc 80 instead of misreading its fields.
40 constant SNAP-TRL-LEGACY-BYTES

\ DICT-SIZE = CFSTK-OFF (= DICT-CAP * DREC record slots) + $1000 control-flow
\ stack; the code area follows at DBASE+DICT-SIZE inside the REGION.
\ Grown $61000 -> $C1000 with DICT-CAP 8192 -> 16384 (the native test closure
\ needs ~9.5k records; dot habu-gate-runner-entry-81c84af0).
\ Grown $C1000 -> $181000 with DICT-CAP 16384 -> 32768 and REGION $400000 ->
\ $800000 (dot habu-lprot-narrow-protection-03cc8d7f): both sides grew because
\ maki peaked ndict 16347/16384 (dict side full) AND the code area measured ~92%
\ of the 4 MB split via LATEST/XREF (code side full). Narrowed dict-poke flips
\ (LPROTREC) keep the seal-time internal-mark cost off the grown region, so the
\ Keep DICT-CAP/CFSTK-OFF/
\ DICT-SIZE/HIDX-SLOTS/HIDX-BYTES in step.
\ Grown $181000 -> $301000 with DICT-CAP 32768 -> 65536 and REGION $800000 ->
\ $A00000 (dot habu-seeded-words-invisible-c7505a49). The derivation is under
\ DICT-CAP; both sides grew for the same reason as last time, and the code side
\ grew BECAUSE the dict side did - see CODE-BAND:BYTES below.
$301000 constant DICT-SIZE
\ CODE-BAND:BYTES: what is left of REGION for emitted code, [DBASE+DICT-SIZE,
\ DBASE+REGION). NAMED, not left as a subtraction nobody performs: the two bands
\ share one budget, so every dictionary growth spends the code band's bytes, and
\ that is invisible while only REGION and DICT-SIZE have names. It cost this lane
\ a near miss - lifting DICT-CAP alone would have left 36 KB of code band and
\ traded `dictionary full` for `code space full` at the same suite.
\ The former 10 MiB region was sized from a Maki load. It left 7,335,936 code
\ bytes and failed the guarded core rebuild at EM-COMPILE-UNDEF. A core rebuild
\ also retains its running compiler while compiling the replacement. The fixed
\ 32 MiB region gives these two generations 30,404,608 code bytes. Qualification
\ measures both generations and their live build peak; it does not resize this
\ budget at runtime. Anonymous pages are populated on demand, and snapshots
\ write only the used span [SDB, CP).
package CODE-BAND
public
REGION DICT-SIZE - constant BYTES
;package
48 constant DREC
16 constant DNAME-INL
1 constant OWNER-API-PUB-WID
2 constant OWNER-API-PRI-WID
3 constant FIRST-DYNAMIC-WID
\ --- the wordlist cell's two non-wordlist values ---------------------------
\ A record's wordlist cell is HALF THE HASH INDEX'S KEY (habu1.f C-HIDX-INS
\ keys a slot on the folded name XOR this cell), and it is written once, at
\ publication, BEFORE the record is inserted - which is what lets a probe with a
\ caller's wid find the row or prove it absent.
\
\ NAMESPACE keeps that rule: a package's own row carries it from birth, so the
\ row is on this key's chain and LFIND's qualifier probe finds it there.
\ RETIRED breaks it. src/habu/xref.f XREF-RETIRE stamps it on the cell of a
\ record that is ALREADY in the table, so the row stays on the chain of the wid
\ it was published under. Every lookup keyed on a REAL wid still agrees with a
\ scan (the row's cell no longer matches, so both skip it), but a lookup keyed
\ on RETIRED itself cannot be answered by the table at all - and retiring one
\ name twice puts two rows under the key, which an insert-once table has no slot
\ shape for. habu1.f BSWL therefore keeps its linear scan for exactly this wid.
\
\ Both live in a package rather than beside the constants above because they are
\ new: the surrounding global surface is this file's packaging debt (dot
\ habu-give-layout-f-315df2ca), and a name that has no bare callers yet has no
\ reason to join it. `package SNAP-RELOC` further down is the same shape.
package DICT-WL
public
-1 constant NAMESPACE
-2 constant RETIRED
;package

\ WID:MAX is the largest value a wordlist id may take; it bounds every
\ WID-indexed table and the snapshot's registry validation.
package WID
public
$FFFFFFFE constant MAX
;package
\ ---- which definer made this record ------------------------------------------
\ WHY A RECORD HAS TO SAY THIS AND WHY NOTHING ELSE CAN. A `constant` and a
\ `create`d word are compiled into a body that pushes one decided value and
\ returns, and a caller that mentions one means that value - not a call to it.
\ Whether a record is one of those is a fact of the DEFINER, known when the
\ record is made and at no later moment: the value lives only in the
\ instructions the definer emitted, and the rule below about the region forbids
\ recovering it by reading them - "an ordinary integer may hold any value at
\ all". Running the word is the honest way to obtain the VALUE (src/compiler/
\ native/dict.f says why), but running an ordinary word to find out whether it
\ is one of these is not a question, it is the answer's own side effects. So the
\ definer stamps its kind here, in the record it just made, and every reader
\ asks the record.
\
\ The two-bit kind is an enum, not independent flags.
\ A `constant` pushes a number; a `create`d or `variable` word pushes an address
\ of the DATA region, which a snapshot must move with the region. A compiler
\ that folded the second as if it were the first would bake an address a restore
\ cannot relocate, so the kinds travel separately all the way to the literal.
\ CAST: uses the remaining kind for a declared identity retype. Its real body
\ remains callable, but a compiled mention only renames the checked value.
\
\ THE ONE WRITER THAT FALSIFIES A STAMP CLEARS IT. `does>` patches the created
\ word's RET into a branch to a clause body (habu2.f DOESPATCH:EMIT), and from
\ that instant the record no longer pushes its address and nothing else: it runs
\ whatever the clause says. That patch clears these bits in the same window it
\ writes the branch in, so a stamped record is one whose body is still the
\ definer's own.
\
\ Claimed band: bits 50-51 of the record flags cell [16], below DNAME-MIN-IN
\ (52-59) and above the narrowed name-length field (bits 0-49); native length
\ reads clear the top 14 bits (LSLI 14 / LSRI 14), and the AOT seed carries the
\ pair as a byte of its compact record (src/habu/aot-capture.f) so a stamp
\ survives the round trip exactly as the min-in byte does.
\
\ The recovery CAST: definer stamps the same kind, so bootstrap/cg's name
\ readers clear the same fourteen bits. Capture carries the kind unchanged.
package DKIND
public
$0004000000000000 constant VAL       \ kind 1: the body pushes a decided number
$0008000000000000 constant ADDR      \ kind 2: the body pushes its DATA address
VAL ADDR or constant CAST           \ kind 3: a declared identity retype
VAL ADDR or constant MASK
;package
$0003FFFFFFFFFFFF constant DNAME-LEN-MASK
\ DNAME-MIN-IN (bits 52-59): certified minimum input arity in cells, poked at
\ certification time (checker RECMI latch -> publish tails / seal-time
\ internal-mark pass; dot habu-habu-certified-words-84e84eaf). LFIND folds the
\ byte into x13 bits 8-15; EM-INTERPRET-FIND fails closed BEFORE the BLR when
\ the interpret stack holds fewer cells, so a certified word can never consume
\ below-base garbage at bare top level. 0 = unmarked (unchecked words and
\ words without signatures: the documented boundary). Compiled calls inside
\ checked words are checker-proven and carry no guard. Claimed band: bits
\ 52-59 of the record flags cell [16], below IMM/EXT/WIDE/INT (bits 60-63)
\ and above the definer-kind pair (bits 50-51) and the narrowed name-length
\ field (bits 0-49); native length reads clear the top 14 bits
\ (LSLI 14 / LSRI 14).
$0FF0000000000000 constant DNAME-MIN-IN-MASK
$1000000000000000 constant DNAME-IMM
$2000000000000000 constant DNAME-EXT
\ DNAME-WIDE (bit 62): the word's recorded stack effect carries a
\ wider-than-cell layout value in some row, so executing it at INTERPRET level
\ would land a multi-cell bundle on the untyped interpret stack where scalar
\ dup/drop/swap silently corrupt it (dot habu-tfam-12-interpret-10b385b1).
\ LFIND folds the bit into x13 bit 3; EM-INTERPRET-FIND and interpret ' fail
\ closed on it. Set by xref.f XREF-WIDE-MARK; the checker marks at signature
\ record time once the sequenced src/core/checker.f half lands. Compile-mode
\ calls inside checked definitions are unaffected (pass-2 lowers them).
$4000000000000000 constant DNAME-WIDE
\ DNAME-INT (bit 63): engine-internal executable word - a COLON record defined
\ by the engine-prefix source with no checker-known effect (no certified or
\ trusted signature and no primitive axiom). Set by the seal-time marking pass
\ (src/core/internal-mark.f) so the executable top-level name universe equals
\ the checker's; data records (create/variable/constant, does>-instances) are
\ exempt - push-only bodies, auto-trusted by C-CALL-TRUST-LASTC-* whenever a
\ hook is installed. LFIND folds the bit into x13 bit 4; EM-INTERPRET-FIND and
\ interpret ' fail closed on it with `hb: internal engine word: <token>` +
\ rc 70 (dot habu-hb-crash-bare-c5be6634). Compile-mode references (explicitly
\ unchecked user code, TRUSTED: bodies, hide.f refresh shims) are unaffected:
\ those are declared trusted boundaries.
$8000000000000000 constant DNAME-INT
\ DICT-CAP: dictionary record slots.
\ 65536 exceeds the move-wide imm16 field, so the DICT-CAP comparison sites in
\ src/habu/habu2.f and bootstrap/cg/forth.fs load it with LIT64 - the same
\ treatment HIDX-SLOTS $10000 already needed. LIT64 emits ONE instruction for a
\ single-chunk value, so the sites cost what MOVZ cost, and asm.f ?IMM16 dies
\ `asm: 16-bit immediate out of range` on a missed one, so none can drift.
65536 constant DICT-CAP
$300000 constant CFSTK-OFF
24 constant CF-REC
8 constant CF-LOCN
16 constant CF-LOCF
\ Control-flow stack region [CFSTK-OFF, DICT-SIZE): cell 0 is the depth counter,
\ then CF-REC-byte records. CFSTK-REGION-CAP is how many records fit above the
\ counter without spilling into the JIT code area at DBASE+DICT-SIZE. LCFPUSH
\ rejects (named rc-70) at depth == CFSTK-DEPTH-MAX, BEFORE the write, so a push
\ can never overflow the region (dot habu-cap-native-control-a5669829; the
\ opposite-direction sibling of the LCFPOP orphan-underflow guard). The cap is
\ min(region capacity, a sane ceiling) and always exceeds the checker's CFS cap
\ 31 (CF-PUSH marks UNCK past 31), so no checker-certified nesting is rejected.
DICT-SIZE CFSTK-OFF - 1 cells - CF-REC / constant CFSTK-REGION-CAP   \ 170 records fit
256 constant CFSTK-SANE-MAX                                          \ forward sanity ceiling
CFSTK-REGION-CAP CFSTK-SANE-MAX min constant CFSTK-DEPTH-MAX         \ 170 = min(region, sane)

\ Profiler counter band: one 64-bit sample counter per dictionary slot, reserved
\ at the very top of the DATA region [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE).
\ Sized from DICT-CAP so it always covers every slot BPROF-ON zeroes and EMIT-PROF
\ indexes (NDICT never exceeds DICT-CAP); src/habu/prof.f derives the band base
\ offset as DATA-SIZE - PROF-CNT-BYTES. The band opens with PROF-STATE-BYTES of
\ the profiler's own cells (sample total and limit, the two buckets, the
\ dictionary base recorded by prof-on, its alternate stack, the dump count) and
\ the counters follow. Grows in step with DICT-CAP with no magic byte count, so
\ the band can never fall short of the slots it serves.
64 constant PROF-STATE-BYTES
DICT-CAP cells PROF-STATE-BYTES + constant PROF-CNT-BYTES

$400000 constant SOURCE-ARENA-CAP
SOURCE-ARENA-CAP constant IBUFSZ
20 constant DATA

0 constant DP-CELL
8 constant HND-CELL
16 constant LOCN-CELL
24 constant LOCF-CELL
$3000 constant LOCNAMES
24 constant LOC-REC
\ A LOC-REC is an 8-byte bare-name length followed by the name bytes; a bare
\ name wider than the field is refused by C-LBRACE-STORE-ONE (habu2.f) before
\ it is stored. checker.f LOC-NAME-W and the LLOCWIDEMSG text state the same 16.
LOC-REC 8 - constant LOC-NAME-CAP
64 constant LOC-RECS          \ LOCNAMES holds this many LOC-RECs ($600 bytes); checker.f LOC-CAP states the same 64
\ --- Friend arena (TFAM 2b-i): one contiguous write-protected band
\ [FRIEND-ARENA, FRIEND-ARENA+FRIEND-ARENA-LEN) holding the boot-seal latch plus
\ every checker/wordlist crown-jewel cell (CUR/WIDN/HOOK/DEF-WL, the TRUSTED:
\ TSIG/TCSIG/CRSIG signature cells, the package PKG-* cells, and the DEFER-*
\ cells). The latch cell IS the arena base: it holds 0 while the engine loads its
\ own canonical source (range guard inert) and FRIEND-ARENA-LEN once SEAL-FRIEND
\ runs at the end of the cold prefix. Self-sealing: post-seal any raw write into
\ the band — including the latch itself — is trapped fail-closed, so the seal is
\ a one-way monotonic latch. The band sits BELOW DATA-START, so allot/,/c,/the DP
\ heap (bounded >= DATA-START by DP-CHECK) can never reach it; only sinks that
\ store to a computed address (! c! +! atomic* patch32 snap-rebase, and syscall
\ write buffers) carry the runtime range check. The old scattered slots were
\ since reclaimed: $2780..$27A0/$27C0..$27E8 are free again after the pass-2
\ transaction moved into TXN-STATE-OFF; $27A8 remains CMM-CELL below ($1A0 stays
\ free). One more guarded band, the constructor protected-WID registry below,
\ is checked by the same PROT-GUARD.
\ The 18th cell (SEAL-NDICT-CELL, $A8) holds the seal-time ndict watermark (TFAM
\ 2b-iii). The latch is sealed EARLY (EMIT-SEAL-FRIEND, before the engine's own
\ checker/xref/stdlib source is even evaluated), so the watermark is captured
\ later by SEAL-CAPTURE (habu1.f BSEALCAP) tokens: a baseline at the end of
\ xref.f plus the cold-prefix assembler's token at the true engine-prefix end
\ (after script-argv.f), once ndict is the full engine boundary and no user
\ record exists yet.
\ The dictionary-truncation words (HIDE-DEFS-FROM/FORGET-DEFS-FROM, xref.f) reject
\ a post-seal FORGET below it. It lives inside the sealed band so user source
\ cannot lower the watermark to bypass the guard. ---
$20 constant FRIEND-ARENA               \ arena base offset within the DATA region (x20)
$90 constant FRIEND-ARENA-LEN           \ 18 cells: latch + 16 crown jewels + seal-ndict watermark
FRIEND-ARENA constant FRIEND-LATCH-CELL \ 0 = friend on/open, FRIEND-ARENA-LEN = sealed
$A8 constant SEAL-NDICT-CELL            \ seal-time ndict watermark (TFAM 2b-iii); 0 until SEAL-CAPTURE

\ ENGINE-ERROR:BAD-TAG is the runtime exit status when a compiled MATCH reaches its invalid-tag
\ fallback (TFAM 10 slice 3, docs/type-families.md §16/§24). The compiler emits a
\ self-contained die (write "hb: bad <family> tag\n" to fd 2 + NR-EXIT-GROUP) with
\ NO normal continuation at the tail of every MATCH. A well-typed scrutinee never
\ reaches it; a forged tag (TRUSTED constructor) exits deterministically with this
\ code. 85 is free repo-wide (the fixed engine exits are 64/67/69/70/71/74/75/76/
\ 77/78/83/84/127); it sits above the seal codes in the runtime-exit family.
67 constant UNCAUGHT-RC                 \ deterministic exit status for an uncaught top-level throw (BTHROW
                                        \ THROW-NOREC): the raw code was exit_group'd and kernel-masked to
                                        \ 8 bits, so a multiple of 256 exited 0 silently - fail-open. 67 is
                                        \ free repo-wide (64/70/71/74/76/78/83/84/127 are the other fixed
                                        \ engine exits; 69/77 collide with checker/lint codes).
$28 constant CUR-CELL
$30 constant WIDN-CELL
$38 constant HOOK-CELL
$40 constant DEF-WL-CELL
$48 constant TSIG-A-CELL
$50 constant TSIG-U-CELL
$58 constant TCSIG-A-CELL
$60 constant TCSIG-U-CELL
$68 constant CRSIG-A-CELL
$70 constant CRSIG-U-CELL
$78 constant PKG-PUB-CELL
$80 constant PKG-PRI-CELL
$88 constant PKG-PARENT-CELL
$90 constant PKG-REC-CELL
$98 constant DEFER-META-CELL
$A0 constant DEFER-XT-CELL
\ DEFER-MAGIC: the first cell of the meta trailer `defer` writes just past a
\ deferred word's code, with the address of that word's dispatch cell in the
\ cell after it. It is what tells a reader holding a dictionary record whether
\ that record is a defer's: a record's code is followed by whatever the next
\ definition put there, and an ordinary integer can hold any value at all, so
\ the trailer is recognised by this word and never by the shape of what stands
\ there.
\ It lives here rather than beside the emitter that writes it because there are
\ now two readers - habu2.f C-DEFER-META-WRITE and C-DEFER-TARGET-META write and
\ check it while the engine is being built, and src/compiler/native/dict.f checks
\ it at run time, when the native chain compiles an `is`. Two files stating one
\ magic number is one drift away from a chain that stores through an address it
\ read out of a record that was never a defer's.
$4842444546455201 constant DEFER-MAGIC
\ CMFAM-CELL: resolved construct family id, live only between the family and
\ variant operand tokens of one `construct` form (CMM-CELL state 1 -> 2; TFAM
\ 10 slice 2). Eager family resolution at the family token means no operand
\ string is stashed across a possible REPL line refill. $1B0 has no exact user
\ and no covering ranged region (GTOD-SCRATCH is $1E0..$1F0; the seal suite's
\ deliberate poke hole is $1A0 — left alone).
$1B0 constant CMFAM-CELL
\ MATCH-lowering compile state (TFAM 10 slice 3, docs §16). All DATA-relative
\ (x20), in the reclaimed $B0..$1B0 free band above the friend arena ($20..$B0)
\ and below CMFAM-CELL ($1B0) — rg-verified unused (the seal-suite poke hole $1A0
\ is left alone; the fam stack tops out at $D0+CMFR-MAX*8 = $1A0). CMBK-CELL is a
\ 64-bit branch-kind bitstack (J-OF pushes 0, EM-ADT-MATCH-OF pushes 1, J-ENDOF
\ pops+checks) so ENDOF re-arms the match token machine (CMM=4) only for a MATCH
\ variant branch, never a CASE arm or a nested case/match ENDOF — the compiler
\ analogue of the checker's CF-ENDOF-DISPATCH frame-kind routing. CMTAG/CMPADS
\ hold the pending variant (tag,M-p) between a variant token and its OF (never
\ nested: no token falls between them). CMFR is the nesting fam stack indexed by
\ CMFRD (match depth); a level's fam feeds later variant resolution and the
\ ;MATCH bad-tag family-name die. Definition-scoped: CMFRD/CMBK cleared at
\ colon/TRUSTED: entry and by EM-RESET-COMPILE-STATE alongside CMM-CELL.
$B0 constant CMBK-CELL                  \ ENDOF branch-kind bitstack (0=case arm, 1=match branch)
$B8 constant CMTAG-CELL                 \ pending MATCH variant tag (VAR -> OF)
$C0 constant CMPADS-CELL                \ pending MATCH variant zero pads M-p (VAR -> OF)
$C8 constant CMFRD-CELL                 \ MATCH nesting depth (0 = not in a match)
$D0 constant CMFR-OFF                   \ MATCH fam stack base (one cell per open match)
26 constant CMFR-MAX                    \ levels: $D0..$1A0 = 26 cells (checker caps CF frames at 30)
$1B8 constant BODYLEN-CELL
$1C0 constant RBASE-CELL
$1C8 constant LOOPSP-CELL
STACK-ABI:BASE-CELL constant S0-CELL
$3640 constant REPLH-CELL
$3648 constant RSAVCP-CELL
$3650 constant RSAVND-CELL
$3658 constant RSAVDP-CELL
$3660 constant RSAVSP-CELL
$3668 constant RRECP-CELL
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
$3688 constant PEND-CELL
$3690 constant TKA-CELL
$3698 constant TKL-CELL
$36A0 constant INP-CELL
$36A8 constant INE-CELL
$36C0 constant BPA-CELL
$36D0 constant BPTAB-OFF
$37E8 constant BPWBASE-CELL
$37F0 constant BPWN-CELL
$43C0 constant EVAL-TOP-CELL  \ current native-stack evaluator frame, zero at rest
STACK-ABI:EVAL-BYTES constant EVAL-FRAME-SIZE
$40 constant EVAL-PREV
$48 constant EVAL-PKG
\ EVAL-INB: the outer evaluate's input-buffer START, saved beside INP ([frame+0])
\ and INE ([frame+8]) so a nested evaluate restores it. It is the last free slot
\ of the frame: PKGSNAP ends at EVAL-PKG + PKGSNAP-USE + 8 = $78 and
\ STACK-ABI:EVAL-BASE opens at $80, so the frame does not grow.
$78 constant EVAL-INB

\ --- refusal location band (dot habu-name-the-file-70acbf10) --------------------
\ Every engine load refusal that reaches the LCOMPILEDIE tail names the source it
\ was reading: `hb: <message> at <path>:<line>`. Three cells carry what the tail
\ needs, and they are read by emitted engine code exactly the way
\ INCLUDE-EVALERR-CELL already is.
\
\ PATH-CELL / PATHLEN-CELL: address and byte length of the INNERMOST OPEN include
\ frame's path, or 0/0 when no file is open (tty REPL, `-e`, the boot prefix).
\ src/core/include.f SOURCE-ROOT:PUSH copies the resolved path into the frame it
\ just mapped and publishes it here; POP republishes the parent frame's, so the
\ pair always describes the file the interpreter is inside. The address is the
\ frame's, not INCLUDE-PATH's, because a nested include overwrites that buffer.
\
\ INB-CELL: the START of the buffer INP walks, so the tail can count newlines in
\ [INB, INP) and report a line. habu1.f B-EVAL sets it and saves the outer value
\ in EVAL-INB. It is NOT beside INP/INE: $36B0 is regalloc.f FRFREE-CELL and
\ $36C8 is address-cells.f INDEX-CELL, so the run there is full.
\
\ WHERE THEY GO: the $2800..$3000 band this file documents as free header space
\ (the user return stack moved to its own guarded mapping, STACK-ABI). Swept for
\ a claimant across src lib tools test bootstrap. Below $7FF8, so the tail names
\ each with a `DATA <off> LDR` 12-bit scaled immediate, and below DATA-START, so
\ no compiled source can reach them and DATA-START does not move.
package SRCLOC
public
$2800 constant PATH-CELL
$2808 constant PATHLEN-CELL
$2810 constant INB-CELL
;package

\ $2780..$27A8 (TSIG/TCSIG/CRSIG) relocated into the friend arena above.
\ RPKG-* / PKGRESYNC-CELL (dot habu-recovery-pkg-scope-e0bd98e2): the REPL-line
\ analogue of the evaluator frame’s package snapshot. EM-REPL-READ (LREAD) snapshots the five
\ live package-scope cells at line-start; EM-REPL-RECOVER (LRREC) restores them so a
\ compile error typed at the tty REPL rolls the open-package scope back to the
\ line-start scope, alongside the existing RSAVCP/RSAVND/RSAVDP/RSAVSP rollback.
\ PKGRESYNC-CELL is armed by both recovery legs and drained once at LMAIN
\ (EM-PKG-RESYNC): when the restored engine scope is global it resets the checker's
\ own package scope (checker-end-package) so engine and checker stay in step. These
\ six cells sit in the reclaimed $2780..$27C0 band (rg-verified unused repo-wide,
\ documented free above); small DATA-relative offsets, direct LDR/STR.
$2780 constant RPKG-CUR
$2788 constant RPKG-PUB
$2790 constant RPKG-PRI
$2798 constant RPKG-PARENT
$27A0 constant RPKG-REC
$27C0 constant PKGRESYNC-CELL
$27B0 constant DOESB-CELL
$27B8 constant TRUSTED-CELL
$37D0 constant EVALD-CELL
$37D8 constant EVALERR-CELL
$37E0 constant LMAINP-CELL
$3C88 constant TASK-TCB-CELL
$3C90 constant TASKS-LIVE-CELL
$3C98 constant HIDXP-CELL
\ HIDX:CLAIMS: how many hash-index slots have ever been claimed - live rows
\ plus the garbage that rollback churn leaves behind (a slot whose stale index
\ is re-covered when NDICT regrows). LHIDXADD compacts the table in place the
\ moment this count crosses HIDX:LOAD-MAX, so the count is exact by
\ construction: C-HIDX-INS increments it exactly when it claims an EMPTY slot,
\ and only the rebuild (which re-derives it from live [0,NDICT)) ever lowers
\ it. Lives at $27C8 in the reclaimed $27C0..$27D8 PKG-* band (rg-verified
\ unused repo-wide); engine-emitted code is its only writer. In a package for
\ the same reason DICT-WL is: new names have no reason to join this file's
\ global packaging debt.
package HIDX
public
$27C8 constant CLAIMS
;package
\ ---- the region's three write bands ------------------------------------------
\ The region mapping holds three things a compile bracket writes, and they are
\ far apart: the dictionary records at [DBASE, DBASE+CFSTK-OFF), the control-flow
\ stack at [DBASE+CFSTK-OFF, DBASE+DICT-SIZE), and the emitted code at
\ [DBASE+DICT-SIZE, DBASE+REGION). All three are RX at rest, so a bracket must
\ make writable every band it writes and flip each back at the close.
\
\ EACH BAND IS TRACKED SEPARATELY BECAUSE ONE RANGE CANNOT HOLD THEM. A single
\ [lo, hi) covering the record being published and the code at CP spans the whole
\ dictionary between them - which is what the flip used to do, from DBASE every
\ time. Measured on a source-prefix boot (macos-arm64, dot
\ habu-narrow-the-boot-9637c873): 14044 flips per boot over a mean 2.06 MB, and
\ the boot went from 335 ms to 243 ms when the bands were split.
\
\ THE SYSCALLS ARE THE SMALLER HALF OF THAT. Replaying the engine's own captured
\ mprotect sequence against a bare mapping costs 67-80 ms, so the rest is what a
\ wide flip does to everything it covers: RW->RX->RW over [DBASE, CP) drops the
\ page-table entries of the megabytes of JIT'd code the engine is EXECUTING and
\ of the dictionary it is reading, and every one of them faults back in. The
\ boot's minor faults fell from 122968 to 29277 with the bands, and its system
\ time from 0.14 s to 0.05 s. The replay could not see that, because nothing was
\ running inside its mapping - the same reason a contiguous record..code range
\ measures as no gain there and is still the wrong shape here.
\
\ THE EXTENT OF A BAND IS DECLARED BY ITS WRITER, never guessed from NDICT. A
\ fixed window of pages around &dict[NDICT] would be a lucky value: the AOT seed
\ publishes LAOTNREC records in one bracket and C-PACKAGE-EXISTING-PRIVATE
\ rewrites a record found anywhere below the watermark. Both already falsify it.
\ A writer that forgets to declare its span writes to an RX page and the engine
\ dies in the crash handler (exit 134) - which is what makes the declaration set
\ complete by construction: `install --force` drives every definer over the whole
\ prefix before any suite runs.
\
\ PROT:WINDOW / PROT:WLO: the end and start addresses of the RW window a bracket
\ has open over the CODE band, or 0 when it is at rest. RECORDING BOTH ENDS is
\ what makes the narrow flip sound: the brackets emit between open and close, so
\ CP differs at the two ends and a recomputed close would flip a different range
\ than the open made writable, orphaning RW pages above it. The open records its
\ range here and the close flips exactly that range back, so the region tail above
\ the window is never RW at all - it is RX from boot and stays RX.
\ PROT:RLO / PROT:RHI: the same pair for the DICTIONARY-RECORD band, or 0 when no
\ record span has been declared in this bracket.
\ PROT:CF: a latch, not a range - the CONTROL-FLOW band's extent is the constant
\ [CFSTK-OFF, DICT-SIZE), so there is nothing per-bracket to record but whether it
\ is open. Dot habu-move-the-control-c7de6246 retires this band by moving the
\ control-flow stack out of the protected region; when it lands, this cell and
\ PROT:LCF go with it.
\ src/habu/habu1.f EMIT-PROT-WINDOW owns every read and write of all five;
\ engine-emitted code is their only writer.
\
\ WHERE THEY HAD TO GO, RE-DERIVED ON THE MERGED LAYOUT. WINDOW/WLO/RLO take the
\ last three free cells of the reclaimed $27C0..$27E8 band, which then has none
\ left. RHI and CF go ABOVE THE EVALUATOR POINTER BAND, at $47C0/$47C8 in the unclaimed
\ run above the reserved evaluator-pointer band ($43C0..$47C0)
\ and the lowering transaction state ($5000) - swept for a claimant across src lib
\ tools test maki bootstrap before taking them. They are NOT in the $40C8..$43A8
\ gap: that run is reserved for widening the protected-WID bitmap, which cannot be
\ split, and AOT-WINDOW:T0-CELL/D0-CELL/B0-CELL already took its top three cells to
\ keep the rest contiguous. All five sit below $7FF8, the ceiling AOT-WINDOW measured for a
\ cell a compiled routine names directly (`DATA <off> LDR` is a 12-bit immediate
\ scaled by eight), which every one of these is: the band bodies read them with
\ exactly that form. All five are below DATA-START, so no compiled source can reach
\ them. In a package for the same reason HIDX:CLAIMS above is.
package PROT
public
$27D0 constant WINDOW
$27D8 constant WLO
$27E0 constant RLO
$47C0 constant RHI
$47C8 constant CF
;package

\ AOT-SIG:POOL-CELL / AOT-SIG:LEN-CELL: where the baked SIGNATURE POOL is and how
\ long it is, published by the seed (habu2.f EM-SEED-AOT) at the moment it
\ registers the captured records, and read by the checker's lazy intake when a
\ definition names a seeded word the checker has no effect for. Zero until a seed
\ runs, which reads as "no pool" and leaves the intake a no-op - so an engine with
\ nothing captured behaves exactly as it did.
\ THE POOL IS NOT COPIED ANYWHERE. It is __text, mapped for the life of the
\ process, and an offset into it means the same thing at every boot; a copy would
\ buy nothing and cost the whole pool's DATA.
\ WHERE THEY GO: the next two cells of the same unclaimed run PROT:RHI/CF took,
\ $47D0/$47D8, swept for a claimant across src lib tools test maki bootstrap
\ before taking them, below $7FF8 and below DATA-START for the reasons above.
\ THE CHECKER MIRRORS THESE TWO NUMBERS (src/core/checker.f CK-AOT-SIG-*-OFF)
\ because src/core/checker.f loads BEFORE this file in every host that has both -
\ the same reason CK-PKG-REC-OFF is mirrored there. The mirror is not left to
\ prose: test/aot-sig-pool-suite.f reads both names out of a booted engine, where
\ both are live, and refuses a disagreement.
package AOT-SIG
public
$47D0 constant POOL-CELL
$47D8 constant LEN-CELL
;package

\ AOT-SPAN:TABLE-CELL / N-CELL / BASE-CELL: where the baked CODE-SPAN TABLE is,
\ how many rows it has, and the address the blob was copied to, published by the
\ seed (habu2.f EM-SEED-AOT) the moment the blob lands. The image ships no
\ dictionary record for a word nothing can name, so the only account of that
\ word's code is a row of this table, and src/habu/aot-closure.f is the reader:
\ hb-build retargets every PC-relative branch against the span that owns it, and
\ a displacement into a span nothing accounts for has nowhere to go.
\ A ROW IS A BLOB OFFSET, so the base is what turns it into an address of the
\ code this boot actually copied. Zero in all three reads as "no stripped spans",
\ which is what an engine with nothing captured, and a whitebox image that kept
\ every name, both are.
\ WHERE THEY GO: the $600..$800 header hole BODYBUF-OFF names as free space,
\ directly above GENIO-ABI's band, swept for a claimant across src lib tools test
\ maki bootstrap and read back as zero out of a booted engine. NOT the run above
\ BOOT-LAYOUT:HEAP-START-CELL, which reads free in the source and is not: that is
\ stack-abi.f's $47E8..$4810, then the transaction and USER-BAND. All three are below
\ $7FF8 and below DATA-START, like their neighbours.
package AOT-SPAN
public
$660 constant TABLE-CELL
$668 constant N-CELL
$670 constant BASE-CELL
;package

\ SIGNAL-ABI: the baked async-signal-safe handler stub (src/habu/crash.f
\ EMIT-SIGNAL-HANDLER, label LSIGH) and the one word it reads. STUB-CELL is the
\ stub's runtime address, which is what a program hands sigaction as its
\ sa_handler; FD-PTR-CELL is the ADDRESS of the fd word, so a program stores its
\ self-pipe's write end through a published pointer instead of spelling where
\ that word lives; FD-CELL is that word's own offset, claimed here so nothing
\ else takes it. The two published cells are written at boot beside the crash
\ handler's own installation (habu2.f EM-STARTUP-RUNTIME-STATE, aot-lib.f
\ EMIT-ENTRY), the way the AOT-SPAN cells are written by the seed. The ENGINE's
\ boot also CLEARS the fd word there, because snap-lib.f SND-COPY carries DATA
\ from offset zero, so an image written while a program had the stub armed
\ carries that program's descriptor number; a stripped image needs no clear,
\ since its DATA is a fresh mapping and its restore starts at DATA-START.
\
\ THE STUB NEVER READS THE FD WORD THROUGH x20. A handler runs on whichever
\ thread the kernel hands the signal to, and that thread's x20 is its own task
\ region - lib/task.f PREPARE maps a fresh zeroed region per task and
\ TASK-REGION-INIT copies only the cells it names - so `DATA FD-CELL LDR` would
\ read a different word on every task and zero on a new one. The stub reaches
\ the word by its ABSOLUTE address instead, DATA-VA + FD-CELL baked in as a
\ literal: DATA-VA is a MAP_FIXED address (src/os/<target>/layout.f, and
\ EM-MMAP-DATA-REGION refuses a boot the kernel answered elsewhere), so that one
\ address names the SAME word for the life of the process whatever task is
\ running. That is the LOCK-CELL class of process-wide cell, and it is why
\ FD-PTR-CELL publishes an address rather than an offset. Every task's region
\ carries a copy of the FD-CELL slot that nothing reads; that is the price of
\ keeping the word inside the one fixed mapping the image already owns.
\
\ THE TWO PUBLISHED CELLS ARE READ FROM THE MAIN TASK, like the AOT-SPAN cells
\ they follow: a spawned task's region is fresh and carries neither, so a
\ library reads them once where the boot wrote them and keeps the two values in
\ its own storage. What it keeps is process-wide - one stub address and one word
\ address - so every task then arms and disarms the same word.
\
\ WHERE THEY GO: the next three cells of the $600..$800 header hole BODYBUF-OFF
\ names as free space, directly above AOT-SPAN's band, swept for a claimant
\ across src lib tools test maki bootstrap and read back as zero out of a booted
\ engine. Below $7FF8, the ceiling for a cell the boot addresses directly as
\ `DATA <off> STR`, and below DATA-START, so no compiled source can reach them.
package SIGNAL-ABI
public
$678 constant STUB-CELL
$680 constant FD-PTR-CELL
$688 constant FD-CELL
;package

\ BOOT-LAYOUT:HEAP-START-CELL: the DP-heap floor of the RUNNING engine, as a DATA
\ offset, stored by habu2.f EM-DATA-INIT out of the same DATA-START it hands DP.
\ It is the engine stating its own layout, so a tool that has to classify the
\ engine's DATA can stop reading a SOURCE constant for it.
\
\ WHY AN ENGINE HAS TO SAY THIS. tools/native-build.f splits the host's declared
\ address rows into the engine cells below the heap and the retired heap above it,
\ and the host's floor is not the loaded tree's: a tree whose reserved bands grew
\ (XTCELL-CAP here, TIER-PROV:SPANS next) moves DATA-START, while the host that
\ builds that tree does not move with it. Classified by the source constant, the
\ host keeps its OWN heap rows and the capture refuses them by value
\ (aot-capture.f ACAP-TARGET-REFUSE, exit 74; dot
\ habu-classify-captured-addr-68fcc1df). Measured 2026-09-12: the cold-build seed's
\ floor was 958280 while the grown tree read 1220424, and the seed's heap row at
\ DATA+1140613 fell between them.
\
\ AN OFFSET, NEVER AN ADDRESS. The cell is a function of this engine's layout and
\ of nothing else in the process, so two builds by one host agree on it and it
\ carries no build-time transient. It also needs no relocation, which is why it is
\ not itself a declared address cell.
\
\ ZERO MEANS "THIS ENGINE PREDATES THE CELL", and it is the answer rather than a
\ sentinel: no engine's heap starts at offset 0 -- the whole reserved band is below
\ DATA-START -- and the anonymous DATA mapping every boot starts from reads zero
\ everywhere. tools/native-build.f names its fallback for that case.
\
\ WHERE IT HAD TO GO: the next cell of the same unclaimed run PROT:RHI/CF and
\ AOT-SIG:POOL-CELL/LEN-CELL took, swept for a claimant across src lib tools test
\ maki bootstrap before taking it, below $7FF8 and below DATA-START for the reasons
\ above. Sitting in the FIXED header matters more here than for its neighbours: the
\ consumer reads this cell out of a host whose reserved bands differ from its own,
\ so the offset it reads has to be one no band growth can move.
package BOOT-LAYOUT
public
$47E0 constant HEAP-START-CELL
;package


\ GENIO-ABI: where the generic I/O layer (docs/genio.md, lib/genio.f) keeps the
\ routing the ENGINE itself has to read. Everything else about a device -- its
\ eight operations, its private state, its handle -- belongs to the library and
\ lives in ordinary dictionary storage; only these cells cross into engine code.
\
\ A DEVICE INDEX, NEVER AN ADDRESS. OUT-CELL and IN-CELL hold a small integer:
\ zero is the built-in terminal path and 1..DEVICES names a row of the write
\ table below. The distinction matters because these two cells are PER TASK -
\ DATA is swapped for each task, and lib/task.f TASK-REGION-INIT copies them
\ into a new task's region so a task inherits its creator's devices. A task
\ region is an ordinary anonymous mapping: nothing canonicalises a code address
\ copied into it, so a per-task cell holding an execution token would survive a
\ snapshot or an AOT capture pointing into the builder's region. An index
\ survives anything, and the seed mirror is one constant.
\
\ THE WRITE TABLE is the one place an execution token does appear, and it is
\ in the engine header, where SNAP-RELOC already canonicalises declared cells:
\ lib/genio.f publishes a device's write operation into its row through `xt!`,
\ exactly as src/habu/repl.f publishes its line reader into REPLH-CELL, and
\ clears every row again through IMAGE-LIFECYCLE before a capture is taken. The
\ engine reads a row only to reach a device write; it never learns what a device
\ is. DEVICES is deliberately small: a process has a terminal, perhaps a
\ connection or two and a capture buffer, and a fixed table costs no allocation
\ on a microcontroller. TASK-REGION-INIT copies the rows into a task's region as
\ well, which is sound for the reason RBASE-CELL's copy is: a task region lives
\ and dies inside one process and is never snapshotted or captured, so a
\ process-lifetime code address in it never has to survive anything.
\
\ BUSY-CELL guards the output funnel against re-entry. A device write is
\ ordinary Habu code and may itself reach `type` -- an error report inside a
\ socket write is the obvious way -- so while a device write runs, the funnel
\ takes the terminal path instead of calling itself.
\
\ ACTIVE-CELL says which device an operation is running FOR, and it exists
\ because the eight operations are shared code: two TCP devices are the same
\ eight quotations over two different connections, and a quotation cannot
\ capture the device it belongs to (docs/forth.md refuses a local inside one).
\ An operation therefore asks this cell for its own row and reads its state
\ there. The funnel publishes it around the write it calls, and lib/genio.f
\ publishes and restores it around every other operation.
\
\ WHERE THEY HAD TO GO: the $600..$800 header hole BODYBUF-OFF below names as
\ free space, left over from the DO/LOOP frame band that became a guarded
\ mapping. Swept for a claimant across src lib tools test maki bootstrap before
\ taking it, and read back as zero out of a booted engine. The run above
\ BOOT-LAYOUT:HEAP-START-CELL that reads free in the source is NOT: src/habu/
\ stack-abi.f already owns $47E8..$4810, and USER-BAND ($5300..$7BF8) is what
\ lib/task.f `+USER` hands out. This band ends at $660 and leaves
\ the word-frame lane's $7D0/$7D8 and the $3800 null cell untouched. Every cell
\ is below $7FF8 -- the ceiling for a cell a compiled routine names directly as
\ `DATA <off> LDR`, which the funnel's first instruction is -- and below
\ DATA-START, so no compiled source can reach it and DATA-START does not move.
package GENIO-ABI
public
8 constant DEVICES                 \ device rows; a device index is 1..DEVICES
$600 constant OUT-CELL             \ per-task current output device index (0 = terminal)
$608 constant IN-CELL              \ per-task current input device index (0 = terminal)
$610 constant BUSY-CELL            \ output-funnel re-entrancy guard (0 = idle)
$618 constant ACTIVE-CELL          \ device index an operation is running for (0 = none)
$620 constant WRITE-OFF            \ DEVICES cells: row i-1 = device i's write operation
WRITE-OFF DEVICES 8 * + constant END
;package


\ EVALREC-CELL: runtime address of the eval-frame throw-unwind entry (LEVALREC,
\ habu2.f), set at startup like LMAINP-CELL so the throw primitive (a leaf prim that
\ cannot name emit-time labels) can branch to it. It must sit in a DATA slot no
\ compiled source ever writes: $3A00..$3C88 is the lib/ffi-abi.f FFI buffer block
\ (FFI-BUF-OFF etc.), $3C88..$3CA0 is the task TCB cells; the protected-WID registry
\ count/table follow at $3CB8..$40C0 — so these $3CA0..$3CB8 slots are free engine
\ cells between the FFI block and the registry.
$3CA0 constant EVALREC-CELL
\ AOT-SEED-DONE-CELL: one-shot flag set the first time the post-cold-prefix AOT
\ seed runs (EM-COMPILE-EXIT, at the end of the engine prefix stream), so neither
\ the user stream's own exhaustion nor REPL re-entry can seed a second time. It is
\ the seed's only guard: the seed itself now runs on every boot. Lives in the
\ $3CA0..$3CB8 free engine gap between the FFI block and the registry count cell.
$3CA8 constant AOT-SEED-DONE-CELL
\ BOOT-SRC:USER-END: the end of the USER source stream while the ENGINE PREFIX
\ stream is running, and 0 once that stream has been installed (one-shot). In a
\ package for the same reason HIDX:CLAIMS is: new names have no reason to join
\ this file's global packaging debt.
\
\ A boot reads two top-level streams out of one buffer. C-SOURCE builds the engine
\ prefix and then the user program (piped bytes, `s" f.f" required` rows, or the
\ baked LSRC) into the same mapping; the shared cold-prefix routine publishes the
\ prefix end as INE, and the mode publishes the buffer end here. The interpreter
\ therefore reaches "source exhausted" (EM-COMPILE-EXIT, LEX0) at the END OF THE
\ ENGINE PREFIX on every boot, which is the one point where the engine is complete
\ and no user token has run yet: the AOT seed goes there, and then LEX0 installs
\ [INE, USER-END) as the next stream and re-enters the interpreter, the same move
\ EM-REPL-READ makes for a typed line.
\
\ This replaced AOT-SEED-ARM-CELL, which armed the seed at the interactive REPL
\ entry alone (dot habu-decide-arm-the-5234727b, USER RULING 2026-08-11: one
\ dictionary surface for every boot mode). Arming everywhere without splitting the
\ streams would have been arming nothing: measured, the seed fired AFTER the batch
\ program it was supposed to serve. Zeroed by DATA-INIT for every boot.
package BOOT-SRC
public
$3CB0 constant USER-END
;package
\ --- protected-WID registry (TFAM 2b-v): count cell + u32 table. Records the WIDs of
\ sealed system / generated constructor packages created in the friend window;
\ PROT-WID? membership (habu1.f) gates the sealed-WID guards.
\
\ SHAPE: a WID-INDEXED BITMAP. Bit w of the $400-byte band at PROT-BITS-OFF is set
\ exactly when wordlist w is protected, so membership and insertion are O(1) and the
\ set can hold ANY subset of the WIDs the engine can index. It replaced a flat
\ 256-entry u32 append-only table (dot habu-replace-the-protected-ca920a8f). That
\ table's capacity was the number of public ADT families ONE PROCESS may declare --
\ a quantity unrelated to any resource the program controls -- and it was scanned
\ linearly by every sealed-WID guard. It filled at 246/256 on master, so the maki
\ suite's next public family died with an uncaught 7169 that named an innocent enum
\ in whatever file happened to declare next. Raising the number (16 -> 256 once
\ already, dot habu-seal-protwid-cap-6f1c9d2b) only moves that cliff.
\
\ CAPACITY: PROT-WID-MAX is now a WID BOUND, not a slot count: the highest wordlist
\ id + 1 that can ever be protected. The band is the SAME $3CC0..$40C0 the 256-slot
\ table occupied, so nothing above it moves, and $400 bytes of bitmap index 8192 WIDs.
\ prot-wid-add names the bound itself when handed a WID at or above it. Growing the
\ bound later means widening the band upward, exactly as the 16 -> 256 raise did,
\ and bumping UNCGH-CELL -- but the cells above $40C8 are NOT free any more:
\ $40C8..$41C8 is FFI:FFI-REG-LEN-BUF-OFF (lib/ffi-abi.f), and APP-ENTRY:XT-CELL,
\ the AOT capture window and the evaluator-pointer band follow from $43A0
\ (measured 2026-09-16, when a null-cell placement at $40C8 broke test/seal.f).
\ A wider bitmap has to move the FFI length buffers first.
\
\ The band stays engine-reserved -- no compiled source writes it, the DP heap is
\ bounded >= DATA-START (above it) and snapshot saves it. [PROT-REG-OFF,
\ +PROT-REG-LEN) is a SECOND range checked by PROT-GUARD, rejecting user data stores
\ into the tag cell, the bitmap, or the uncaught-throw hook. The code-emit sinks
\ cp!/ndict! (habu1.f BCPSET/BNDSET) ARE range-guarded too: each PROT-GUARDs the
\ address it redirects a write to, so a post-seal cp!/ndict! into either band fails
\ closed at the sink. ---
$3CB8 constant PROT-REG-TAG-CELL        \ bitmap-shape tag; capture requires this exact runtime shape
$50574249544D4150 constant PROT-REG-TAG \ "PWBITMAP": written by every path that publishes the band
$3CC0 constant PROT-BITS-OFF            \ protected-WID bitmap base
8192 constant PROT-WID-MAX              \ WID bound: bits 0..8191 span $3CC0..$40C0, the exact band the 256-slot table held
PROT-WID-MAX 8 / constant PROT-BITS-BYTES
PROT-BITS-OFF PROT-BITS-BYTES + constant PROT-BITS-END
PROT-REG-TAG-CELL constant PROT-REG-OFF \ second PROT-GUARD band base (= tag cell)
PROT-BITS-END 1 cells + PROT-REG-OFF - constant PROT-REG-LEN  \ $410: tag + bitmap + UNCGH-CELL = $3CB8..$40C8
\ UNCGH-CELL: runtime address of the uncaught-top-level-throw reporter (LUNCAUGHT,
\ habu2.f), stored at boot (EM-STARTUP-RUNTIME-STATE) beside RRECP/EVALREC so the leaf
\ BTHROW primitive (which cannot name a habu2.f label) can branch to it when a throw
\ reaches THROW-NOREC with no handler and no REPL. Moved $3D00 -> $40C0 (above the grown
\ protected-WID band); not read live at build time so the relocation is safe.
\ Like EVALREC/AOT-SEED it is a fixed engine cell no compiled source writes (the mmap'd
\ DATA region is zero until boot).
$40C0 constant UNCGH-CELL
\ Compact AOT dict record: five u32 words (blob-off/public-WID, code-len/private-WID,
\ name-off, flags|min-in, wid). The name-off word is a full u32 so the deduped name
\ pool can outgrow 64 KiB with the captured window; the compiler chain needs ~51 KiB
\ of names where the metabuild REPL window needs 953 bytes.
20 constant AOT-CREC-ROW
\ The deduplicated pool admits at most DICT-CAP entries, each containing a
\ one-byte length and at most 255 name bytes. Storage grows to the used size.
DICT-CAP 256 * constant AOT-NAMES-CAP
\ Dict-name hash index: slots stay a power of 2 (LFIND probes with the
\ HIDX-SLOTS 1 - mask) and 2x DICT-CAP so the load factor stays <= 50%;
\ bytes = slots * 4 (u32 entries). Grown with DICT-CAP 32768: HIDX-SLOTS $10000
\ ($10000 = 65536 exceeds MOVZ imm16, so the three habu1.f probe sites load it
\ with LIT64), HIDX-BYTES $40000. Grown again with DICT-CAP 65536: HIDX-SLOTS
\ $20000, HIDX-BYTES $80000 - the 2x identity is why DICT-CAP stays a power of
\ two rather than taking the first value that clears the headroom rule.
$20000 constant HIDX-SLOTS
$80000 constant HIDX-BYTES
\ HIDX:LOAD-MAX: the claimed-slot count at which LHIDXADD compacts the table
\ in place instead of letting chains grow toward a full wrap. Three quarters of
\ the file: strictly above DICT-CAP ($10000), so a compaction always lands the
\ count back at NDICT with room to spare and can never itself be at the bound,
\ and strictly below HIDX-SLOTS, so an insert always meets an empty slot within
\ its chain and the full-wrap path is structurally unreachable (it dies loudly
\ if reached; nothing silently disables the index any more - CG-25).
package HIDX
public
$18000 constant LOAD-MAX
;package
$36B8 constant FRCLM-CELL
$37F8 constant SNAP-CELL
\ NULL-PTR-CELL-OFF: the engine's one permanently zero cell. Checked code cannot
\ name a raw address, so it reads a pointer's NUMBER only by subtracting a null
\ pointer, and src/core/pointer-storage.f NULL-PTR reads that zero from here.
\
\ WHY IT IS A HEADER OFFSET AND NOT A `create`d BODY. It used to be one, in the
\ DP heap, so every mention compiled an absolute movz/movk chain to a heap
\ address. A stripped AOT image restores only the program's own DATA span, so the
\ linker refused the chain by name ("aot: address refers to data outside the
\ restored span", aot-closure.f DATA-ADDRESS!) and nothing whose closure reached
\ NULL-PTR could be linked -- which, since CDIGEST:NATIVE-SLOT? is on the path of
\ every checked wide fetch, was every such program. From the DATA base the
\ mention is `x20 + $3800`, which carries no address chain to refuse, and a
\ stripped image's fresh MAP_ANON DATA reads the same zero the engine does.
\
\ WHERE IT HAD TO GO: the next cell of the enumerated single-cell run that ends
\ at SNAP-CELL ($37F8), below the FFI argument buffers that open at $3A00. Swept
\ for a claimant across src lib tools test maki bootstrap, and BOTH kinds of
\ claim were checked, not only `$XXXX constant` lines: $40C8, the cell the
\ protected-WID comment below still calls free gap, is in fact
\ FFI:FFI-REG-LEN-BUF-OFF, and from $43A0 up sit APP-ENTRY:XT-CELL, the AOT
\ window and the evaluator-pointer band; the task rows are USER-BAND, above the
\ transaction.
\ Below DATA-START like every cell here, so the DP heap cannot reach it and the
\ snapshot carries it; the anonymous DATA mapping every boot starts from reads
\ zero everywhere, which is the whole of what this cell needs.
\
\ PROTECTION IS ADDRESS-CELLS:LOCK-CELL'S, no more and no less: a reserved header
\ cell is below DATA-START, so allot/,/c, cannot reach it, but a store to a
\ computed address can -- LOCK-CELL is written exactly that way, by atomic-cas,
\ on purpose. What keeps checked source off THIS cell is the same thing that kept
\ it off the heap body: pointer-storage.f REG-PROTECTs the NULL-PTR-CELL record,
\ so the name is DNAME-INT and no checked source can compile a mention of it.
\ test/internal-word-gate.f pins that, pins that the cell still reads zero, and
\ pins this offset against the copy pointer-storage.f has to spell for itself.
$3800 constant NULL-PTR-CELL-OFF
$1D8 constant SSCR-CELL
$1E0 constant GTOD-SCRATCH
$200 constant VSP-CELL
$210 constant VTAG-OFF
$250 constant VVAL-OFF
32 constant VSMAX


\ NCOMP-DISPATCH:XT-CELL is the OPTIMIZING compiler's dispatch, read only when
\ TIER-CELL selects tier 1. It sits at $358, which was the legacy jit.f
\ BEGIN-snapshot depth cell until that storage moved to JIT-SNAP (further down
\ this file). The two cannot share the address now that the legacy compiler is
\ reachable again: the first `BEGIN` in a tier-0 definition wrote this cell and
\ the two declaration-owner cells above it.
\
\ Tier 1 still has no fallback: an unset XT-CELL dies at NCOMP-EMIT:LOAD with the
\ seed-integrity code. TIER-CELL selects which compiler runs; it never rescues a
\ broken one.
package NCOMP-DISPATCH
public
$358 constant XT-CELL

\ TIER-CELL ( 0 | 1 ) -- which compiler the `:` handlers dispatch to.
\
\   0 = tier 0, the legacy JIT `:`/`;` compiler (LCOMPILE and its closure).
\       Every `--load` and the REPL. It is the value a fresh DATA page starts
\       at, so an engine that selects nothing runs the JIT.
\   1 = tier 1, the IR pipeline reached through XT-CELL (NCOMP:COMPILE).
\       Selected deliberately with `1 set-tier` by the paths that construct an
\       executable -- an executable carries only optimised code.
\
\ Tier 1 must be selected BEFORE the first definition on such a path is
\ evaluated: NCOMP's READ-PRIOR exits on GLUE-UNKNOWN when it meets a body the
\ JIT compiled, so a tier-1 build has to be a fresh load and never an image laid
\ over a tier-0-loaded prefix. Selecting it at the top of the path, ahead of any
\ source, makes that hold by construction rather than by review.
\
\ Read by emitted code as `DATA TIER-CELL LDR` (12-bit scaled immediate), so it
\ stays 8-aligned and below $7FF8 -- see the measurement recorded at the AOT
\ window cells further down this file.
$370 constant TIER-CELL
\ Latched at the colon; an immediate may only select the NEXT definition.
$388 constant DEF-TIER-CELL
\ The outer executable-build scope saves the ordinary selection once. Nested
\ scopes keep tier 1 until the outer finally restores it.
$390 constant BUILD-DEPTH-CELL
$398 constant BUILD-TIER-CELL
\ One owner record keeps every engine declaration registrar paired with the
\ active compiler, including while the source dictionary/checker is replaced.
\ The record is DATA; its fields are execution tokens of existing private
\ checker operations, with no new checked-callable trust alias.
$360 constant DECL-CELL
$368 constant TARGET-DECL-CELL
CHECKER-OWNER-ABI:RAW-OFF constant DECL-RAW-OFF
CHECKER-OWNER-ABI:EFFECT-OFF constant DECL-EFFECT-OFF
CHECKER-OWNER-ABI:DEFER-OFF constant DECL-DEFER-OFF
CHECKER-OWNER-ABI:CAST-OFF constant DECL-CAST-OFF
CHECKER-OWNER-ABI:USING-OFF constant DECL-USING-OFF
CHECKER-OWNER-ABI:PACKAGE-OFF constant DECL-PACKAGE-OFF
CHECKER-OWNER-ABI:PUBLIC-OFF constant DECL-PUBLIC-OFF
CHECKER-OWNER-ABI:PRIVATE-OFF constant DECL-PRIVATE-OFF
CHECKER-OWNER-ABI:END-PACKAGE-OFF constant DECL-END-PACKAGE-OFF
CHECKER-OWNER-ABI:TRANSFER-OFF constant DECL-TRANSFER-OFF
CHECKER-OWNER-ABI:SOURCE-ROW-OFF constant DECL-SOURCE-ROW-OFF
CHECKER-OWNER-ABI:SOURCE-CON-OFF constant DECL-SOURCE-CON-OFF
CHECKER-OWNER-ABI:EXPORT-OFF constant DECL-EXPORT-OFF
CHECKER-OWNER-ABI:WIDE-OFF constant DECL-WIDE-OFF
CHECKER-OWNER-ABI:RESET-OFF constant DECL-RESET-OFF
CHECKER-OWNER-ABI:CAPTURE-OFF constant DECL-CAPTURE-OFF
\ Everything below is the OPTIMIZING front end's half of the same record, and it
\ is why the record exists at all for tier 1. That front end IS the checker's
\ scan: the scan feeds the source tape the elaborator reads, answers the does>
\ split, records the per-call-site facts selection needs, and its effect store is
\ what the dialect asks about every name a body calls. Reaching any of it by NAME
\ bound the compiler to the checker instance the engine was BUILT against, so a
\ run that REPLACES the source dictionary had its own files scanned by the
\ retired instance -- which has no symbol for a name the new source just defined
\ (src/core/check-hook.f's package-private CHECK-RC was the first to bite, and a
\ product engine could not rebuild the tree at tier 1 at all).
\
\ APPENDED, so no existing offset moves. checker-owner-abi.f owns the offsets
\ and derives its byte count from the last field. checker.f checks that count
\ against the cells it commits. These names preserve the engine-facing API.
\ --- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one
CHECKER-OWNER-ABI:CHECK-OFF constant DECL-CHECK-OFF
CHECKER-OWNER-ABI:TAPE-INSTALL-OFF constant DECL-TAPE-INSTALL-OFF
CHECKER-OWNER-ABI:TAPE-ARM-OFF constant DECL-TAPE-ARM-OFF
CHECKER-OWNER-ABI:TAPE-DISARM-OFF constant DECL-TAPE-DISARM-OFF
CHECKER-OWNER-ABI:TAPE-ADVANCE-OFF constant DECL-TAPE-ADVANCE-OFF
CHECKER-OWNER-ABI:DOES-CHECK-OFF constant DECL-DOES-CHECK-OFF
CHECKER-OWNER-ABI:DOES-IN-OFF constant DECL-DOES-IN-OFF
CHECKER-OWNER-ABI:DOES-OUT-OFF constant DECL-DOES-OUT-OFF
CHECKER-OWNER-ABI:DOES-WIDE-OFF constant DECL-DOES-WIDE-OFF
CHECKER-OWNER-ABI:USIG-TRUNCATE-OFF constant DECL-USIG-TRUNCATE-OFF
\ --- the finalized per-call-site facts the scan recorded
CHECKER-OWNER-ABI:CALL-CELLS-OFF constant DECL-CALL-CELLS-OFF
CHECKER-OWNER-ABI:CALL-GLUE-OFF constant DECL-CALL-GLUE-OFF
CHECKER-OWNER-ABI:CALL-MATCH-OFF constant DECL-CALL-MATCH-OFF
CHECKER-OWNER-ABI:CALL-QUOT-IN-OFF constant DECL-CALL-QUOT-IN-OFF
CHECKER-OWNER-ABI:CALL-QUOT-OUT-OFF constant DECL-CALL-QUOT-OUT-OFF
\ --- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one
CHECKER-OWNER-ABI:TRUST-DECL-OFF constant DECL-TRUST-DECL-OFF
CHECKER-OWNER-ABI:PARSE-IMM-OFF constant DECL-PARSE-IMM-OFF
\ --- the effect-store query group: EFFECT-QUERY resolves a name into the
\ instance's query state and every reader below reads THAT state, so all of them
\ have to reach the same owner or a reader answers about another instance's query
CHECKER-OWNER-ABI:EFFECT-QUERY-OFF constant DECL-EFFECT-QUERY-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-N-OFF constant DECL-EFFECT-DIN-N-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-N-OFF constant DECL-EFFECT-DOUT-N-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-CELLS-OFF constant DECL-EFFECT-DIN-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-CELLS-OFF constant DECL-EFFECT-DOUT-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-SLOT-OFF constant DECL-EFFECT-DIN-SLOT-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-SLOT-OFF constant DECL-EFFECT-DOUT-SLOT-OFF
CHECKER-OWNER-ABI:EFFECT-DIN-QUOT-OFF constant DECL-EFFECT-DIN-QUOT-OFF
CHECKER-OWNER-ABI:EFFECT-DOUT-QUOT-OFF constant DECL-EFFECT-DOUT-QUOT-OFF
CHECKER-OWNER-ABI:EFFECT-QUOT-UP-OFF constant DECL-EFFECT-QUOT-UP-OFF
CHECKER-OWNER-ABI:EFFECT-RET-NEUTRAL-OFF constant DECL-EFFECT-RET-NEUTRAL-OFF
CHECKER-OWNER-ABI:EFFECT-QUOT-SIMPLE-OFF constant DECL-EFFECT-QUOT-SIMPLE-OFF
CHECKER-OWNER-ABI:EFFECT-CATCH-CELLS-OFF constant DECL-EFFECT-CATCH-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-EXEC-CELLS-OFF constant DECL-EFFECT-EXEC-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-FINALLY-CELLS-OFF constant DECL-EFFECT-FINALLY-CELLS-OFF
CHECKER-OWNER-ABI:EFFECT-MATCH-CELLS-OFF constant DECL-EFFECT-MATCH-CELLS-OFF
CHECKER-OWNER-ABI:CTL-DEAD-OFF constant DECL-CTL-DEAD-OFF
CHECKER-OWNER-ABI:WF-W-AT-OFF constant DECL-WF-W-AT-OFF
\ --- what the record a definition publishes needs from the checker
CHECKER-OWNER-ABI:REC-MIN-IN-OFF constant DECL-REC-MIN-IN-OFF
CHECKER-OWNER-ABI:REC-WIDE-PUBLISH-OFF constant DECL-REC-WIDE-PUBLISH-OFF
\ --- the scan whose verdict nobody enforces, with the render suppressed for it.
\ A TRUSTED: body is scanned only to fill the tape, so the suppression has to
\ happen in the instance that renders; the counter itself is unreachable from a
\ baked compiler, which is the whole reason this is an operation and not a cell.
CHECKER-OWNER-ABI:CHECK-UNJUDGED-OFF constant DECL-CHECK-UNJUDGED-OFF
\ Family/variant ids and all metadata about them share the live source owner.
CHECKER-OWNER-ABI:FAMILY-MATCH-OFF constant DECL-FAMILY-MATCH-OFF
CHECKER-OWNER-ABI:FAMILY-CON-OFF constant DECL-FAMILY-CON-OFF
CHECKER-OWNER-ABI:FAMILY-VARIANT-OFF constant DECL-FAMILY-VARIANT-OFF
CHECKER-OWNER-ABI:FAMILY-SLOTS-OFF constant DECL-FAMILY-SLOTS-OFF
CHECKER-OWNER-ABI:FAMILY-VARIANTS-OFF constant DECL-FAMILY-VARIANTS-OFF
CHECKER-OWNER-ABI:FAMILY-NAME-OFF constant DECL-FAMILY-NAME-OFF
CHECKER-OWNER-ABI:VARIANT-TAG-OFF constant DECL-VARIANT-TAG-OFF
CHECKER-OWNER-ABI:VARIANT-PADS-OFF constant DECL-VARIANT-PADS-OFF
CHECKER-OWNER-ABI:VARIANT-PAY-CELLS-OFF constant DECL-VARIANT-PAY-CELLS-OFF
CHECKER-OWNER-ABI:VARIANT-PAY-TERMS-OFF constant DECL-VARIANT-PAY-TERMS-OFF
;package


\ BODYBUF-OFF was spelled as the end of the DO/LOOP frame band while that band
\ lived at $600..$800. The frames are a guarded mapping now (STACK-ABI), so this
\ states its own offset: the $600..$800 hole below it is free header space.
$800 constant BODYBUF-OFF
8000 constant BODYBUF-CAP
$568 constant RSP-CELL
$570 constant EXITH-CELL
$578 constant LVD-CELL
$580 constant LVH-OFF
$2C0 constant LVF-OFF
$560 constant LASTC-CELL
$1F0 constant DOESP-CELL
$230 constant CREATEP-CELL
$238 constant QPATCH-CELL
$240 constant QENT-CELL
$248 constant QXH-CELL
\ FRAME-CELL: the open body's link-register frame, in one cell. The JIT is
\ single pass, so `:` cannot know whether the body will call anything; it emits
\ the save at the entry and records the slot here, and every emitter that puts a
\ call in the body ORs bit 0 in. At `;` the cell therefore answers both "where
\ is the entry slot" and "can anything have destroyed x30", which is the whole
\ of what EM-COMPILE-RET has to decide. Bit 0 is free because a code address is
\ four-byte aligned. Zero means no frame is open, so there is nothing to
\ restore and nothing to patch. QFRAME-CELL scopes it across a quotation
\ exactly as QXH-CELL scopes the EXIT chain: a quotation's calls are its own
\ frame's business, not the enclosing body's. Definition-scoped like the Q
\ cells, cleared by EM-RESET-COMPILE-STATE, never live across a snapshot.
\ Both sit in the free header hole below BODYBUF-OFF (rg-verified unused).
$7D0 constant FRAME-CELL
$7D8 constant QFRAME-CELL
$250 constant DEF-TKA-CELL
$258 constant DEF-TKL-CELL
\ CMM-CELL: compile-loop ADT-lowering mode (TFAM 10, docs/type-families.md §16),
\ mirroring the checker's MM token machine: 0 = off; slices 2-3 arm it at a
\ `construct`/`MATCH` keyword so the operand tokens are captured BEFORE the
\ local/keyword/literal/call/undefined dispatch and never hit dictionary lookup.
\ Tested fail-closed at the LCOMPILE head (EM-COMPILE-ADT-MODE): armed with no
\ handler dies deterministically. Definition-scoped: cleared at colon/TRUSTED:
\ entry and by EM-RESET-COMPILE-STATE. Lives at $27A8, the last old CRSIG slot
\ (freed when CRSIG moved into the friend arena) between the reclaimed $27A0
\ slot and DOESB-CELL ($27B0) — rg-verified unused repo-wide. NOTE the low
\ "free hole" $260 is NOT usable: VVAL-OFF ($250) + VSMAX cells spans
\ $250..$350, and DEF-TKA/DEF-TKL survive inside it only because their liveness
\ is confined to the definition NAME token, when the virtual stack is empty.
$27A8 constant CMM-CELL
\ PKG-* ($27C0..$27D8, less HIDX:CLAIMS at $27C8 and PROT:WINDOW/WLO at
\ $27D0/$27D8), the old DEFER-META slot ($27E0, now PROT:RLO), and the old
\ $2780..$27A0 pass-2 cells are reclaimed by the immutable lowering
\ transaction. The final old defer slot is the protected compile-immediate
\ preflight hook. The
\ checker installs it before arming HOOK-CELL; the compiler calls it before
\ every source-defined immediate in a checked body, and 0 is an invalid armed
\ state. It persists through snapshots like HOOK-CELL.
$27E8 constant COMPILE-PREFLIGHT-CELL
\ The retired descriptor-hook slot $27F0 is reused by TOP-HOOK-CELL below.
\ TOP-HOOK-CELL: top-row token hook xt (dot habu-typed-top-engine-2b2e88aa,
\ docs/typed-top-level.md §2.1). 0 = no hook: the interpret dispatch is
\ byte-for-byte today's behavior (tier 0). Installed only through the
\ fail-closed `set-top-check` prim (habu1.f BSETTOPCHECK), which mirrors
\ `set-check`/BSETCHECK's live-code install window; the cell is its own
\ PROT-GUARD band (habu1.f GUARD-SPAN/PROT-GUARD) so a post-seal raw store
\ traps ENGINE-ERROR:SEAL-VIOLATION exactly like the HOOK-CELL crown jewel. It sits in
\ the reclaimed band between TRUSTED-CELL ($27B8) and RSTK-OFF ($2800) and is
\ snapshot-persistent (< DATA-START) like HOOK-CELL. When installed, the
\ interpret dispatch points (habu2.f EM-INTERPRET-FIND / EM-INTERPRET-NUMBER
\ / the pushing string keywords / C-TICK / C-CHAR) emit one pre-continue
\ event per token through LTOPHOOK; the hook's effect is
\ ( ptr u8 n n n -- ): token addr, token len, class (TOP-EV-*), LFIND flags
\ (word/tick classes; 0 for literal classes).
$27F0 constant TOP-HOOK-CELL
\ ENGINE-SNAP-XT-CELL retains the first cold-prefix checker's snapshot-prepare
\ hook while a --build payload loads a second checker copy. Snapshot capture
\ invokes that first-copy hook before serializing DATA; capture and startup
\ clear it so no build-process code pointer reaches a restored image. $27F8 is
\ the final reclaimed cell before RSTK-OFF and is protected with TOP-HOOK-CELL.
$27F8 constant ENGINE-SNAP-XT-CELL
COMPILE-PREFLIGHT-CELL constant ENGINE-HOOK-OFF
3 cells constant ENGINE-HOOK-LEN
\ Top-row event class codes: the protocol between the interpret dispatch and
\ an installed top-row hook. Word/tick events pass the LFIND flag word
\ (bit 0 found, bit 1 DNAME-IMM, bits 8-15 DNAME-MIN-IN); literals pass 0.
1 constant TOP-EV-NUM       \ number literal pushed ( n )
2 constant TOP-EV-STR       \ s" / S\" literal pushed ( ptr u8 n )
3 constant TOP-EV-CSTR      \ c" / C\" counted literal pushed ( ptr )
4 constant TOP-EV-CHAR      \ char literal pushed ( n )
5 constant TOP-EV-TICK      \ ' pushed a found word's xt
6 constant TOP-EV-WORD      \ found word about to execute (pre-BLR)
\ The user return stack is a guarded mapping (STACK-ABI), not the $2800..$3000
\ header band it used to be: a band inside a $8000 header cannot carry an
\ inaccessible page, and an overflow there silently overwrote LOCNAMES. Its base
\ lives in STACK-ABI:RETURN-BASE-CELL and the depth stays in RSP-CELL, so a
\ slot is [RETURN-BASE-CELL] + depth*8. $2800..$3000 is free header space.
STACK-ABI:RETURN-CELLS constant RSTK-CELLS

\ ---- catch/throw handler frame (habu1.f BCATCH/BTHROW, habu2.f
\ EM-EVAL-THROW-RECOVER; bootstrap/cg/forth.fs mirror) ----
\ A HNDF-SIZE machine-stack frame chained through HND-CELL ([DATA+8]). Saves the
\ user return-stack depth (RSP-CELL), loop-stack depth (LOOPSP-CELL), and active
\ data-stack allocation so a caught throw restores the complete caller frame
\ (dot habu-restore-complete-exec-abb8baca). Field offsets are byte offsets from
\ the frame base; every delivery site (BTHROW, LEVLD, seed BTHROW) reads them and
\ validates the sentinel, saved depths and allocation BEFORE any restore store. Keep this block
\ byte-for-byte in step with the bootstrap/cg/forth.fs mirror.
STACK-ABI:CATCH-BYTES constant HNDF-SIZE
\ frame layout: 0 prev-HND | 8 data-sp(x19) | 16 machine-sp | 24 resume-pc
\               32 link | 40 saved-RSP | 48 saved-LOOPSP | 56 sentinel
\               64 saved-base | 72 saved-capacity
STACK-ABI:LOOP-FRAMES constant LOOP-STK-FRAMES
STACK-ABI:CATCH-MAGIC constant CATCH-FRAME-MAGIC

\ Compiler lowering transaction. All mutable pass-2 authority lives in one
\ engine band. The frozen source+certificate lives in a separately mmap'd,
\ maximum-target-page-rounded allocation whose base and capacity are held in
\ the protected state. It is read-only during replay and unmapped at commit.
$5000 constant TXN-STATE-OFF
\ THE DECLARED EXTENT, not a reservation. This used to be $3000, which is where
\ PD-TABLE-OFF had to land rather than anything the transaction owned: the cells
\ end at TXN-LIVE-W-OFF + TXN-LIVE-W-CAP cells = $5300, and $5300..$8000 was
\ 11520 bytes of slack. habu1.f BAND-TAB guards this length, so the slack was
\ guarded too, and PROT-GUARD refused a store anywhere in it - which is why the
\ task-user arena could not be moved there while it read $3000. It is the
\ declared extent now and the DATA-CLAIMS assertion at the end of this file
\ refuses a transaction cell past it, so growing the transaction is a layout
\ edit that fails the build rather than a silent reach into USER-BAND below.
$300 constant TXN-STATE-LEN
$10000 constant PROT-PAGE-MAX          \ = STACK-ABI:PAGE-BYTES; rt.f executes the agreement

TXN-STATE-OFF       constant TXN-ACTIVE-CELL
TXN-STATE-OFF $8  + constant TXN-SRC-A-CELL
TXN-STATE-OFF $10 + constant TXN-SRC-U-CELL
TXN-STATE-OFF $18 + constant TXN-CERT-A-CELL
TXN-STATE-OFF $20 + constant TXN-CERT-U-CELL
TXN-STATE-OFF $28 + constant TXN-BIND-I-CELL
TXN-STATE-OFF $30 + constant P2-CELL
TXN-STATE-OFF $38 + constant TXN-WF-I-CELL
TXN-STATE-OFF $40 + constant P2BODY0-CELL
TXN-STATE-OFF $48 + constant P2INP-CELL
TXN-STATE-OFF $50 + constant P2INE-CELL
TXN-STATE-OFF $58 + constant P2DP-CELL
TXN-STATE-OFF $60 + constant P2W0-CELL
TXN-STATE-OFF $68 + constant P2W1-CELL
TXN-STATE-OFF $70 + constant P2W2-CELL
TXN-STATE-OFF $78 + constant P2W3-CELL
TXN-STATE-OFF $80 + constant P2LOC0-CELL
TXN-STATE-OFF $88 + constant TXN-FETCH-I-CELL
TXN-STATE-OFF $90 + constant TXN-BLOB-A-CELL
TXN-STATE-OFF $98 + constant TXN-BLOB-CAP-CELL
TXN-STATE-OFF $100 + constant TXN-LIVE-W-OFF
64 constant TXN-LIVE-W-CAP

\ --- Task-local library storage (dot habu-give-the-task-57a9243e) ---------------
\ The run the transaction reserved and never used, handed to the libraries that
\ need storage a task owns. It has to be HERE and not where lib/task.f used to
\ hand out rows ($41C8..$43A0, 472 bytes): that run is bounded by
\ APP-ENTRY:XT-CELL and five shipped libraries already claimed 448 of it, and
\ every other candidate below TASK-REGION-BYTES ($10000, the whole a task has)
\ is claimed - PD-TABLE from $8000, USE-BAND above it, then SNAP-RELOC's XTCELL
\ table, which runs past 2.6 MB and would alias in the main thread's region.
\
\ USER-REGION-END is the anchor and does not move. It is where PD-TABLE-OFF has
\ always started, so DATA-START is where it was and no mirrored constant shifts.
$8000 constant USER-REGION-END

\ STRING-ABI is lib/string.f's SB builder, declared here because string.f is
\ baked into the engine and sits BELOW lib/ffi-abi.f, which requires it back:
\ `require lib/task.f` from string.f fails the engine build with an undefined
\ STR= inside ffi-abi's TOK-IS?, so SB can never take a TASK:+USER row and needs
\ a declared offset the way FFI-BUF-OFF and GENIO-ABI do. This lane declares and
\ asserts the band; habu-make-the-shared-0c2bfbc6 moves the builder into it.
\ Carved from the TOP of the run so USER-BAND keeps the growable end.
package STRING-ABI
public
$400 constant SB-BUF-BYTES                  \ lib/string.f SB-CAP
8 constant SB-LEN-BYTES                     \ lib/string.f SB-LEN
SB-BUF-BYTES SB-LEN-BYTES + constant BYTES
USER-REGION-END BYTES - constant START
START constant SB-BUF-OFF
SB-BUF-OFF SB-BUF-BYTES + constant SB-LEN-OFF
USER-REGION-END constant END
;package

\ FMT-ABI is lib/fmt.f's integer render buffer and the four cells its appenders
\ and its fraction helper thread. Declared here for the same reason STRING-ABI
\ is, though not from the same cause: src/habu/habu2.f requires lib/fmt.f for
\ number text, so fmt is inside the ENGINE'S OWN closure. fmt has no require
\ cycle the way string.f does, and an engine built with `require lib/task.f` in
\ fmt.f compiles - but that require pulls lib/task.f, and with it pthread, mmap
\ and the FFI staging tables, into the base image for 52 bytes of scratch.
\ src/habu/native-runtime.f says what belongs in that closure: the compiler,
\ the JIT and the REPL, and the task runtime is none of the three. So the rule
\ is one rule - a module the engine bakes takes a declared band.
\
\ Carved from the TOP of the run, directly below STRING-ABI, so USER-BAND keeps
\ the growable end. EVERY MEMBER IS ITS MEASURED WIDTH; nothing here is rounded
\ up to leave room. The four cells sit at the band base, which is a cell
\ boundary, so they are cell-aligned whatever the render buffer's width is, and
\ the buffer follows them. BYTES is the member sum rounded up to the cell the
\ base needs, and that rounding is the band's only slack: 52 bytes of members
\ in a 56-byte extent, the four above the buffer claimed by nobody.
package FMT-ABI
public
4 constant SCRATCH-CELLS               \ FMT-NUM-U, FMT-IX, FMT-FR, FMT-DV
20 constant NUM-BUF-BYTES              \ lib/fmt.f FMT-NUM-CAP = STR-I64-DIGITS + 1
SCRATCH-CELLS cells NUM-BUF-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
STRING-ABI:START constant END
END BYTES - constant START
START constant NUM-U-OFF
NUM-U-OFF 8 + constant IX-OFF
IX-OFF 8 + constant FR-OFF
FR-OFF 8 + constant DV-OFF
DV-OFF 8 + constant NUM-BUF-OFF
;package

\ FS-ABI is lib/fs.f's per-call slots: the five cells one transfer threads, the
\ stat buffer, the one-byte read probe and the NUL-padded path copy. lib/fs.f
\ is NOT baked into the engine, so the rule above would send it to TASK:+USER
\ rows - except tools/native-build-core.f requires it to BUILD the engine, and
\ a `require lib/task.f` there loads the task runtime into the build tool ahead
\ of the target it is building. A module the engine's own BUILD needs is in the
\ same position as one the engine bakes, so it takes a declared band too.
\
\ IT ALSO COSTS A BOOTSTRAP, which the band above does not. The PRODUCT IMAGE
\ tools/native-build.f emits carries this file's constants, and lib/errors.f's
\ codes, from the tree it was BUILT in; adding a band here does not make an
\ existing one see it. A baked module does not care - `require lib/string.f` or
\ `lib/fmt.f` is a no-op in a booted engine, so only the target build ever
\ compiles them, after this file. lib/fs.f is loaded from source by the build
\ tool, so an engine that predates this band dies `E-UNDEFINED:
\ FS-ABI:STAT-BYTES`, rc 70, before any target work. Build one engine from this
\ declaration and E-FS-BAND alone, then build the tree with it.
\
\ THE WALK STATE IS NOT HERE and WALK-FILES stays single-task: FS-WALK-BUF and
\ FS-DIR-BUF are FS-MAX-DEPTH deep, $8000 and $20000 bytes, fifteen times this
\ whole band. A second task that must walk needs its own walker, not a wider
\ per-task region.
\
\ Sized the way FMT-ABI above is: every member at its measured width, the five
\ cells at the band base so they are cell-aligned whatever the buffers do, the
\ buffers after them, and BYTES the member sum rounded up to the cell the base
\ needs - 1322 bytes of members in a 1328-byte extent.
package FS-ABI
public
5 constant IO-CELLS                    \ FS-IO-FD, FS-IO-LEN, FS-IO-RD, FS-IO-OFF, FS-IO-WR
256 constant STAT-BYTES                \ lib/fs.f FS-STAT-CAP
1 constant PROBE-BYTES                 \ lib/fs.f FS-READ-PROBE-CAP
1025 constant PATHZ-BYTES              \ lib/fs.f FS-PATHZ-CAP = PATH-CAP + 1
IO-CELLS cells STAT-BYTES + PROBE-BYTES + PATHZ-BYTES + constant MEMBER-BYTES
MEMBER-BYTES 7 + 8 / 8 * constant BYTES     \ the band base is a cell boundary
FMT-ABI:START constant END
END BYTES - constant START
START constant FD-OFF
FD-OFF 8 + constant LEN-OFF
LEN-OFF 8 + constant RD-OFF
RD-OFF 8 + constant OFF-OFF
OFF-OFF 8 + constant WR-OFF
WR-OFF 8 + constant STAT-OFF
STAT-OFF STAT-BYTES + constant PROBE-OFF
PROBE-OFF PROBE-BYTES + constant PATHZ-OFF
;package

\ USER-BAND is what lib/task.f `TASK:+USER` hands out, base and bound both.
package USER-BAND
public
TXN-STATE-OFF TXN-STATE-LEN + constant START
FS-ABI:START constant END
;package

\ --- Pre-trust defer pending table (dot habu-engine-pre-trust-77410827) ---
\ `defer NAME ( E )` declared in src/core/checker.f BEFORE `: TRUST`
\ (checker.f:7685) cannot run C-CALL-TRUST-PEND / C-CALL-CHECKER-DEFER, which
\ LFIND `trust`/`checker-defer` (undefined until 5208/7687) and die exit 70. When
\ they are absent C-DEFER instead COPIES the defer's qualified name + effect
\ signature into a slot of this fixed table; DRAIN-PRETRUST (called once in
\ checker.f right after `: TRUST`) replays both registrations for every slot.
\ Populated and drained entirely inside the checker.f prefix load, so the table is
\ empty by snapshot time; it sits at the TOP of the reserved region and bumps
\ DATA-START (protected-WID growth precedent) so no existing engine offset moves.
\ Overflow (and an over-long name/sig) dies at declaration; a non-empty table at
\ SEAL-CAPTURE dies (undrained backstop) — both fail-closed, named. Slot access
\ uses a computed band base (PD-TABLE-OFF > the DATA-relative scaled-imm range),
\ then small in-slot field offsets. Slot: [0]=name-len [8]=sig-len
\ [PD-NAME-OFF..)=name bytes [PD-SIG-OFF..)=sig bytes.
\ Protection class: UNGUARDED engine scratch (BODYBUF class, not a PROT-GUARD
\ band). A raw user store into it grants nothing beyond the public
\ `trust`/`checker-defer` words — the drain replays only name+sig slot copies —
\ and the table is empty whenever user source runs. test/protection-span.f pins
\ this class at the DATA-START edge.
48 constant PD-CAP                          \ pending slots: stage-2b pre-7687 needs ~20 (B5 + render/snapshot class); headroom
48 constant PD-NAME-CAP                      \ max qualified defer-name bytes per slot
64 constant PD-SIG-CAP                       \ max effect-signature bytes per slot
0  constant PD-NLEN-OFF                       \ in-slot: name length (u64)
8  constant PD-SLEN-OFF                       \ in-slot: sig length (u64)
16 constant PD-NAME-OFF                       \ in-slot: name bytes
PD-NAME-OFF PD-NAME-CAP + constant PD-SIG-OFF \ in-slot: sig bytes
PD-SIG-OFF PD-SIG-CAP + constant PD-SLOT      \ per-slot stride
8 constant PD-SLOTS-REL                       \ slots begin after the u64 band count cell
STRING-ABI:END constant PD-TABLE-OFF   \ band base (= old DATA-START, = USER-REGION-END); [0]=count
PD-TABLE-OFF PD-SLOTS-REL + PD-CAP PD-SLOT * + constant PD-TABLE-END

\ --- Package-scope eval-frame snapshot band (dot habu-recovery-pkg-scope-e0bd98e2) ---
\ Evaluator entry snapshots package/search state in its native stack frame.
\ Clean exit restores using depth; throw recovery also restores package scope.
\ Package/search snapshots are fields of each native-stack evaluator frame.
0  constant PKGSNAP-CUR
8  constant PKGSNAP-PUB
16 constant PKGSNAP-PRI
24 constant PKGSNAP-PARENT
32 constant PKGSNAP-REC
40 constant PKGSNAP-USE


\ --- `using`-scope import band (dot habu-using-import-pkg-a07dd7ba) ---
\ Consumer-side namespace import: `using NAME` makes package NAME's PUBLIC wordlist
\ visible to bare lookup until `;using`, `;package`, or the end of the load file.
\ Live state is a small fixed-capacity stack of public wordlist IDs plus a depth
\ counter in a DATA band whose offset is shared with the checker. The depth
\ cell is the single source of truth for how many usings are live: engine machine
\ code owns it (C-USING pushes, C-END-USING pops, `package`/`;package` and the
\ eval-frame / REPL boundaries save+restore it), and the checker reads the same
\ cell through `data-base USE-DEPTH-CELL +` so its parallel package-name mirror
\ (checker.f CHK-USE-NAMES) is bounded by the identical depth with no separate
\ counter to drift. The depth is always 0 at rest (usings are file-local and closed
\ before seal/snapshot), so the band is transient.
16 constant USE-MAX                       \ concurrent `using` capacity (E-code on overflow)
PD-TABLE-END $400 + constant USE-BAND-OFF  \ preserve the checker-owned using offset
USE-BAND-OFF          constant USE-DEPTH-CELL      \ live using depth (u64)
USE-BAND-OFF 8 +      constant USE-PKG-SAVE-CELL   \ depth saved at `package` open (`;package` restores)
USE-BAND-OFF 16 +     constant USE-RPKG-SAVE-CELL  \ depth saved at REPL line start (recover restores)
USE-BAND-OFF 24 +     constant USE-WIDS-OFF        \ public-wid array base (USE-MAX u64 cells)
USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END

\ --- snapshot relocation bookkeeping (dot habu-relocate-snapshot-region-752042fe) ---
\ Two tables that let a snapshot image be restored at a region address the
\ writing run never saw. Both live in the engine-reserved DATA band below
\ DATA-START, so the ordinary snapshot DATA copy carries them with no new image
\ section, and both are keyed by an OFFSET rather than an address, so their own
\ contents are the same in every run and never need canonicalising.
\ The engine half of this subsystem reopens this package in src/habu/habu2.f and
\ the snapshot writer's half reopens it in src/habu/snap-lib.f.
package SNAP-RELOC
public

\ Exit status for a corrupt call map: the loader found a recorded region-to-text
\ call site that does not hold a call instruction, so the image's region bytes and
\ its call map come from different builds or one of them is damaged. Relocating it
\ anyway would write a wild branch into live code, so the image is refused. It
\ lives here beside BL-RANGE-RC rather than in the
\ src/core/engine-error.f registry for the same reason those two do: the engine
\ emitter reads its exit statuses from this file while it is being compiled, one
\ generation before a new src/core constant would be reachable. 95 is the next
\ free status above that registry's last entry (94), and 96 and 97 follow it.
95 constant CALLMAP-RC
\ Exit status for an overfull address-cell table: more cells were declared to hold
\ a region address than XTCELL-CAP has room for. Continuing would silently drop a
\ cell and leave a stale writer-run address in a restored image, so the engine
\ stops instead.
96 constant XTCELL-RC
\ Exit status for a corrupt address-literal map: the loader found a recorded
\ address-literal site that does not hold the four-instruction MOVZ/MOVK chain the
\ compiler emits there, so the image's region bytes and its literal map come from
\ different builds or one of them is damaged. Rewriting the four immediates anyway
\ would plant a wild address in live code, so the image is refused.
97 constant ADDRMAP-RC
\ Exit status for a declared address cell that does not lie inside DATA. Every row
\ of the table below is a DATA offset, and the three passes that consume it -- the
\ writer's canonicalise, the loader's relocate, and the declaration itself -- index
\ DATA by that offset with no further arithmetic. An offset that is negative or
\ that reaches past the region makes all three read and write memory that is not
\ the cell anyone meant, so it is refused where it is first seen instead of being
\ carried into an image.
98 constant XTBAND-RC
\ Exit status for one address cell declared with both relocation kinds. Treating
\ either declaration as the winner would make one of snapshot or AOT relocation
\ silently wrong, so the common declaration point refuses the conflict.
99 constant XTKIND-RC

\ Call-site map: one bit per four-byte word of the JIT region, recording every
\ call site whose callee lives in the engine's loaded __text instead of inside the
\ region. Those calls are the only instructions whose displacement is not the same
\ in the run that wrote a snapshot image and the run that restores it. A call from
\ one region address to another keeps its distance wherever the region is mapped;
\ a call from the region into __text does not, because the kernel picks the region
\ base and the loader picks the image base independently.
\ A site is recorded when the call is created, at the single call-emit chokepoint
\ (habu2.f EMIT-CEMITBL) and at the AOT call-site patcher (EM-AOT-PATCH-SITES), so
\ nothing ever has to recognise a call again by decoding region bytes -- which
\ could not be done soundly, because a compiled word may carry inline
\ non-instruction data.
\ The snapshot writer rewrites every recorded site to the displacement it would
\ have if the region sat exactly REGION-OFF above __text, and the loader rewrites
\ it again for the distance this run actually got.
\ The size is fixed and derived from REGION, so the map cannot overflow and needs
\ no capacity check: grow REGION and the map grows with it.
REGION 32 / constant CALLMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
USE-BAND-END constant CALLMAP-OFF
CALLMAP-OFF CALLMAP-BYTES + constant CALLMAP-END

\ Address-literal map: the same shape as the call map, one bit per four-byte word
\ of the JIT region, recording the FIRST word of every four-instruction MOVZ/MOVK
\ chain the compiler builds an execution token with. Those are the quotation entry
\ address a `[: ... ;]` pushes and the target a `[']` pushes; the
\ chain names a word's code, which lives either inside the region or in the
\ engine's loaded __text, and neither of those keeps its address between the run
\ that writes a snapshot image and the run that restores it.
\ A separate map rather than a second bit in the call map: a call site and a chain
\ start are different instruction shapes at different addresses, the two passes
\ rewrite completely different fields, and a two-bit call map would cost the same
\ bytes while forcing the already-proven call pass to decode a tag it does not
\ need. Two one-bit maps also let each pass keep the identical bit-scan loop.
\ Membership is recorded where the compiler decides the literal IS an address, of
\ either kind: habu2.f C-CODE-ADDR, C-DATA-ADDR and C-DATA-ADDR-RAW are the three
\ emit points, the native chain's publication seam is the fourth, and the AOT
\ seed's code-literal rebase (EM-AOT-RELOC-CODE) is the fifth.
\
\ THE DATA LITERALS USED TO BE LEFT OUT, and the reason given was true of the
\ wrong consumer. DATA is mapped at a fixed address in every run, so a DATA chain
\ is already correct in the run that restores a snapshot and this map's snapshot
\ reader has nothing to do for it. But the AOT CAPTURE is the other reader, and it
\ has to FIND a DATA chain: the captured blob lands at a different DP in the
\ seeded engine, so a chain-compiled word's DATA address is the metabuild host's
\ and is wrong there. Capture used to find them by scanning the blob for the
\ chain's shape and testing the value against the window's DATA span - a value
\ heuristic of exactly the kind the next paragraph forbids. So both kinds are
\ recorded, and the ONE question this band answers is where a chain starts.
\
\ RECORDING BOTH KINDS COSTS THE SNAPSHOT PASS NOTHING, because that pass is
\ parameterised by band. habu2.f EMIT-ADDRS is called once per band and rewrites a
\ chain only when the address it spells out falls inside that band, leaving one
\ that names neither alone rather than guessing. Its bands are the JIT region and
\ the engine's __text; DATA is mapped MAP_FIXED at DATA-VA, which is far above
\ both in every run and at both ends of a snapshot - the writer folds the region
\ to the RBASE-VA sentinel and the loader maps that sentinel onto the live region
\ base. So a recorded DATA site is visited by both calls and rewritten by neither.
\
\ WHICH KIND a recorded site is, is NOT in this band, and nothing stores it. A
\ consumer holds the spans of the window it is working on, and a recorded site's
\ value falls in exactly one of them - the DATA span lies inside the region mapped
\ MAP_FIXED at DATA-VA, the code span inside the JIT region, and the two cannot
\ overlap. The kind is therefore DERIVED, by a total classification of a value
\ already known to be a real address; what the old value-range scans guessed at
\ was whether a word was a site at all, and this bit is now the only authority on
\ that. A site whose value is in neither span is refused rather than classified
\ (src/habu/aot-capture.f). A second band, or a per-window kind list, would have
\ cost storage to answer a question the spans already answer.
\ Nothing ever recognises a chain by looking at region bytes or at the value a
\ chain carries: a compiled word may hold inline non-instruction data, and an
\ ordinary integer may hold any value at all.
\ Sized from REGION like the call map, so it cannot overflow and needs no capacity
\ check, and keyed by region offset, so its own contents are run-invariant.
REGION 32 / constant ADDRMAP-BYTES        \ one bit per region word (REGION / 4 / 8)
CALLMAP-END constant ADDRMAP-OFF
ADDRMAP-OFF ADDRMAP-BYTES + constant ADDRMAP-END

\ Address-cell table: the DATA offset and kind of every persisted cell that was
\ DECLARED to hold an address. XT cells hold JIT-region addresses; DATA-pointer
\ cells hold addresses in the DATA heap. Region code moves on snapshot restore,
\ while DATA stays fixed, so only XT values need snapshot canonicalisation. Both
\ kinds need the declaration for AOT capture, where the captured DATA window does
\ move and a raw pointer into that window would otherwise escape in sparse bytes.
\ Membership is recorded where the cell's kind is decided, never inferred from
\ what the cell happens to contain: PERSISTED-PTR-VARIABLE registers its
\ DATA-pointer cell,
\ the `defer`/`is` handlers register dispatch cells, and the engine hook cells are
\ registered by name at cold boot (habu2.f). Scanning DATA for values that fall
\ in some address band would be a guess -- an ordinary integer can hold any value
\ at all -- and is deliberately not what this does.
\ Legacy layout: a count followed by XTCELL-CAP tagged offset cells. Bit 63 is the
\ DATA-pointer kind and the remaining bits are the DATA offset. Keeping the kind
\ in the existing row avoids a second registry and does not move DATA-START. The
\ engine appends only an identical declaration once and refuses one cell declared
\ with both kinds.
\ These coordinates freeze the original band and heap floor. New engines use
\ ADDRESS-CELLS' header and inline boot rows in this band, then grow into mmap.
\ XTCELL-CAP bounds only legacy host rows; it is not a runtime declaration limit.
65536 constant XTCELL-CAP
$8000000000000000 constant XTCELL-DATA-TAG
$7FFFFFFFFFFFFFFF constant XTCELL-OFF-MASK
ADDRMAP-END constant XTCELL-N-CELL        \ live count of used rows
XTCELL-N-CELL 8 + constant XTCELL-ROWS-OFF
XTCELL-ROWS-OFF XTCELL-CAP cells + constant XTCELL-END

\ The largest offset a declared cell may carry. All three consumers of a row --
\ the declaration itself (habu2.f EMIT-MARK), the writer's canonicalise
\ (snap-lib.f SND-CANON-XT-CELLS) and the loader's relocate (habu2.f EMIT-XT) --
\ add the offset to a DATA base and read or write the eight bytes there, so the
\ whole cell has to be inside the region. Comparing UNSIGNED against this bound
\ rejects a negative offset in the same instruction, because a negative offset is
\ an enormous unsigned one -- and negative is the shape the real defect produced:
\ a participant table grown into anonymous mmap sat about 4.6e12 bytes BELOW
\ data-base (dot habu-seal-the-declaration-7183177e).
\
\ Containment is the whole rule; the cell is deliberately NOT required to be on a
\ cell boundary. `create` rounds its own field up to a cell (habu2.f LCREATE),
\ but a cell reached through `allot` after byte-sized data, or one inside a
\ copied DATA window, keeps whatever residue it was given, and cell loads and
\ stores at any byte address are well defined on this target. An alignment
\ clause here would reject such a cell on the first `xt!`, which is exactly what
\ it did when this guard was first written with one (before the rounding, two
\ adjacent `create X 4 cells allot` tables measured 7 mod 8).
DATA-SIZE 8 - constant XTCELL-OFF-MAX

;package

\ The application entry is outside compiler scratch storage. Zero preserves
\ ordinary hb CLI routing; a saved application calls this entry before input.
package APP-ENTRY
public
$43A0 constant XT-CELL
;package

\ Capture-window cells retain their existing offsets in the engine DATA layout.
\ AOT-ARM:OPEN writes both coordinates; tier-0 call emission does not read them.
\ T0 is latched when the seed allocates its wordlists: later boot-run entries
\ can allocate more, so the sealed-wordlist gate cannot derive it from WIDN.
package AOT-WINDOW
public
$43A8 constant T0-CELL           \ first wordlist id the seed allocated for the window
$43B0 constant D0-CELL           \ first address of the open window's DATA span
$43B8 constant B0-CELL           \ first address of its code span
;package

\ --- tier 0's BEGIN-snapshot storage -------------------------------------------
\
\ The legacy JIT snapshots its virtual stack at BEGIN and reconciles to it at the
\ back edge (src/habu/jit.f LVSNAP/LVRECON). That storage used to sit at $358 for
\ the depth and $360..$600 for 28 twenty-four-byte frames. The hard cut handed
\ $358/$360/$368 to NCOMP-DISPATCH while the legacy compiler was unreachable, and
\ the frame area had ALREADY grown over LASTC/RSP/EXITH/LVD/LVH ($560..$580) --
\ frames 22 and up landed on live cells. Both faults are latent only while
\ nothing branches to LCOMPILE; selecting tier 0 makes them immediate.
\
\ So the whole thing moves here, an appended band that bumps DATA-START and moves
\ no existing offset -- the growth precedent the pre-trust defer table, the
\ package-scope band and the `using` band all followed.
\
\ THE PRICE OF BEING UP HERE, AND WHY IT IS PAID IN jit.f. This band is far above
\ $7FF8, so an emitted routine can neither name the depth cell with
\ `DATA <off> LDR` (12-bit scaled) nor reach the frames with `ADDI` (12-bit
\ unscaled, max $FFF) -- the two forms jit.f used while the band was low. Both
\ now load the offset with LIT64 and add, which is what habu2.f already does for
\ CFSTK-OFF. It costs two instructions inside LVSNAP and LVRECON, which run once
\ per BEGIN and once per back edge AT COMPILE TIME, and buys a band that no
\ future header growth can collide with.
\
\ Snapshot-carried like every other sub-DATA-START band, which is correct and
\ uninteresting: the frames are live only inside one definition's compile.
\ THE DEPTH CELL STAYS LOW, AND THAT IS NOT AN OPTIMISATION. It is
\ definition-scoped state exactly like VSP-CELL, LVD-CELL and EXITH-CELL, so the
\ colon handlers and EM-RESET-COMPILE-STATE have to zero it in the same breath as
\ those -- a run of `9 DATA <off> STR,` sharing one already-zeroed register. Up in
\ the frame band it would need its own 64-bit literal and a second register,
\ inside a routine documented to clobber x9 only.
\
\ Leaving it out of that run is what leaked: a definition that failed after its
\ BEGIN left the depth raised, 28 such failures walked it to the nesting bound,
\ and the 29th `begin` in the session exited 75 with nothing wrong with it.
package JIT-SNAP
public
28 constant FRAMES                                  \ EMIT-SNAP-NEST-CHECK's bound
24 constant FRAME-BYTES                             \ one frame is (k, p0, p1)
$378 constant SP-CELL                               \ depth; low, beside TIER-CELL
SNAP-RELOC:XTCELL-END constant STK-OFF              \ base of the frame area
STK-OFF FRAMES FRAME-BYTES * + constant END
;package

\ DATA-START: first offset of the user DP heap (allot/,/c,); everything below is
\ engine-reserved state (snapshot saves [0,DATA-START); DP-CHECK bounds the heap
\ >= DATA-START; task-user cells stop at EVAL-TOP-CELL. Its reserved band
\ ends at $47C0, with PROT:RHI/PROT:CF taking the two cells directly
\ above them. The lowering state ends at $8000; the pre-trust defer
\ pending band follows, then the immutable lowering blob lives outside DATA.
\ Sorted, disjoint code intervals. Coordinates are relative to dbase@; a row is
\ (first, end, origin). Unknown is -1, JIT is 0, positively native is 1.
\ Missing coverage is unknown, never evidence that older code was native.
package TIER-PROV
public
8192 constant SPANS
24 constant SPAN-BYTES
JIT-SNAP:END constant OPEN-CELL
OPEN-CELL 8 + constant N-CELL
N-CELL 8 + constant TABLE-OFF
TABLE-OFF SPANS SPAN-BYTES * + constant END
;package

TIER-PROV:END constant DATA-START

\ --- DATA claim map and the layout-time overlap assertion ----------------------
\ WHY IT EXISTS. lib/task.f handed out TASK:+USER rows from $41C8 bounded by
\ TXN-STATE-OFF, because the comments in this file said that run was free. It was
\ not: APP-ENTRY:XT-CELL, the AOT capture window, the evaluator-pointer band,
\ PROT, AOT-SIG, BOOT-LAYOUT and src/habu/stack-abi.f all sit inside it. Five
\ shipped libraries fitted into the 472 bytes before the first of them by luck,
\ and a sixth would have overwritten the AOT window with no diagnostic at all -
\ measured, before the bound was corrected: one $40 row past the mark exits 134
\ at teardown with a register dump.
\
\ A COMMENT COULD NOT CATCH THAT AND DID NOT. This table can. Every claim on the
\ DATA region states its own extent, and CLAIMS-ASSERT refuses an overlapping
\ pair at ENGINE BUILD TIME, naming both. src/habu/habu1.f runs a second check
\ over the same table: every PROT-GUARD BAND-TAB row must BE a declared claim,
\ start and length both, which is the check that would have caught the
\ transaction guarding $3000 of a band whose cells end after $300.
\
\ WHAT IS IN IT: every claim whose extent is DECLARED - a band with a length
\ constant, or a single cell. That is the whole map from $3A00 up, where every
\ library band lives, and all eight BAND-TAB rows. THREE LOW CLAIMS ARE OUT,
\ because their extent exists only as an emitter convention and inventing one
\ would be worse than omitting it: LVH-OFF ($580) and LVF-OFF ($2C0), the
\ DO/LEAVE level arrays LVD-CELL indexes with no declared cap, and the $1A0 seal
\ fixture poke cell, which no constant names. All three are below $800, where no
\ library band reaches.
\
\ DELIBERATE ALIASES ARE ONE ROW, NOT TWO. The friend arena is one row, not the
\ eighteen cells inside it. VVAL-STACK is one row of VSMAX cells: DEF-TKA-CELL
\ and DEF-TKL-CELL live at $250/$258 inside it and survive there because their
\ liveness is confined to the definition name token, when the virtual stack is
\ empty - the note at CMM-CELL above records that trade. Giving them their own
\ rows would assert a conflict the engine takes on purpose.
package DATA-CLAIMS
public

$100 constant MSG-CAP
2 constant ROW-CELLS

create MSG-BUF MSG-CAP allot
variable MSG-U

: MSG-RESET ( -- )
   0 MSG-U ! ;

: MSG+ ( ptr u8 n -- ) {: a u :}
   0 begin dup u < while
      MSG-U @ MSG-CAP < if
         dup a + c@ MSG-BUF MSG-U @ + c!
         MSG-U @ 1+ MSG-U !
      then
      1+
   repeat drop ;

: MSG$ ( -- ptr u8 n )
   MSG-BUF MSG-U @ ;

\ Names live in their own blob, one counted string per row, in row order: the
\ rows have to stay two contiguous cells each, so the bytes cannot sit in them.
\ The blob is ALLOTTED and filled by index, never built with `c,`: an
\ interpreted `s"` appends its own bytes at HERE, so a blob grown with `c,`
\ between two `s"` literals interleaves each name with a copy of itself.
$1000 constant NAMES-CAP
create NAMES NAMES-CAP allot
variable NAMES-U

: NAME-C+ ( n -- ) {: b :}
   NAMES-U @ NAMES-CAP < if
      b NAMES NAMES-U @ + c!
      NAMES-U @ 1+ NAMES-U !
   then ;

: NAME, ( ptr u8 n -- ) {: a u :}
   u NAME-C+
   0 begin dup u < while dup a + c@ NAME-C+ 1+ repeat drop ;

   s" DP-CELL" NAME,
   s" HND-CELL" NAME,
   s" LOCN-CELL" NAME,
   s" LOCF-CELL" NAME,
   s" FRIEND-ARENA" NAME,
   s" CMBK-CELL" NAME,
   s" CMTAG-CELL" NAME,
   s" CMPADS-CELL" NAME,
   s" CMFRD-CELL" NAME,
   s" CMFR-STACK" NAME,
   s" CMFAM-CELL" NAME,
   s" BODYLEN-CELL" NAME,
   s" RBASE-CELL" NAME,
   s" LOOPSP-CELL" NAME,
   s" STACK-ABI-BASE-CELL" NAME,
   s" SSCR-CELL" NAME,
   s" GTOD-SCRATCH" NAME,
   s" DOESP-CELL" NAME,
   s" VSP-CELL" NAME,
   s" VTAG-STACK" NAME,
   s" CREATEP-CELL" NAME,
   s" QPATCH-CELL" NAME,
   s" QENT-CELL" NAME,
   s" QXH-CELL" NAME,
   s" VVAL-STACK" NAME,
   s" NCOMP-XT-CELL" NAME,
   s" NCOMP-DECL-CELL" NAME,
   s" NCOMP-TARGET-DECL-CELL" NAME,
   s" NCOMP-TIER-CELL" NAME,
   s" JIT-SNAP-SP-CELL" NAME,
   s" NCOMP-DEF-TIER" NAME,
   s" LASTC-CELL" NAME,
   s" RSP-CELL" NAME,
   s" EXITH-CELL" NAME,
   s" LVD-CELL" NAME,
   s" GENIO-ABI" NAME,
   s" AOT-SPAN" NAME,
   s" SIGNAL-ABI" NAME,
   s" FRAME-CELL" NAME,
   s" QFRAME-CELL" NAME,
   s" BODYBUF" NAME,
   s" RPKG-SNAPSHOT" NAME,
   s" CMM-CELL" NAME,
   s" DOESB-CELL" NAME,
   s" TRUSTED-CELL" NAME,
   s" SRCLOC-PATH" NAME,
   s" SRCLOC-PATHLEN" NAME,
   s" SRCLOC-INB" NAME,
   s" PKGRESYNC-CELL" NAME,
   s" HIDX-CLAIMS" NAME,
   s" PROT-WINDOW" NAME,
   s" PROT-WLO" NAME,
   s" PROT-RLO" NAME,
   s" ENGINE-HOOK" NAME,
   s" LOCNAMES" NAME,
   s" REPLH-CELL" NAME,
   s" RSAVCP-CELL" NAME,
   s" RSAVND-CELL" NAME,
   s" RSAVDP-CELL" NAME,
   s" RSAVSP-CELL" NAME,
   s" RRECP-CELL" NAME,
   s" ARGC-CELL" NAME,
   s" ARGV-CELL" NAME,
   s" ENVP-CELL" NAME,
   s" PEND-CELL" NAME,
   s" TKA-CELL" NAME,
   s" TKL-CELL" NAME,
   s" INP-CELL" NAME,
   s" INE-CELL" NAME,
   s" FRCLM-CELL" NAME,
   s" BPA-CELL" NAME,
   s" BPTAB" NAME,
   s" EVALD-CELL" NAME,
   s" EVALERR-CELL" NAME,
   s" LMAINP-CELL" NAME,
   s" BPWBASE-CELL" NAME,
   s" BPWN-CELL" NAME,
   s" SNAP-CELL" NAME,
   s" NULL-PTR-CELL" NAME,
   s" FFI-BUFFERS" NAME,
   s" TASK-TCB-CELL" NAME,
   s" TASKS-LIVE-CELL" NAME,
   s" HIDXP-CELL" NAME,
   s" EVALREC-CELL" NAME,
   s" AOT-SEED-DONE-CELL" NAME,
   s" BOOT-SRC-USER-END" NAME,
   s" PROT-REG" NAME,
   s" FFI-LEN-BUFFERS" NAME,
   s" APP-ENTRY-XT-CELL" NAME,
   s" AOT-WINDOW-T0" NAME,
   s" AOT-WINDOW-D0" NAME,
   s" AOT-WINDOW-B0" NAME,
   s" EVAL-POINTER-BAND" NAME,
   s" PROT-RHI" NAME,
   s" PROT-CF" NAME,
   s" AOT-SIG-POOL" NAME,
   s" AOT-SIG-LEN" NAME,
   s" BOOT-HEAP-START" NAME,
   s" STACK-ABI-CAP" NAME,
   s" STACK-ABI-REPL-BASE" NAME,
   s" STACK-ABI-REPL-CAP" NAME,
   s" STACK-ABI-RETURN-BASE" NAME,
   s" STACK-ABI-LOOP-BASE" NAME,
   s" TXN-STATE" NAME,
   s" USER-BAND" NAME,
   s" FS-ABI" NAME,
   s" FMT-ABI" NAME,
   s" STRING-ABI" NAME,
   s" PD-TABLE" NAME,
   s" USE-BAND" NAME,
   s" SNAP-CALLMAP" NAME,
   s" SNAP-ADDRMAP" NAME,
   s" SNAP-XTCELL" NAME,
   s" JIT-SNAP-FRAMES" NAME,
   s" TIER-PROV" NAME,

create TAB
   DP-CELL                        ,  1 cells ,
   HND-CELL                       ,  1 cells ,
   LOCN-CELL                      ,  1 cells ,
   LOCF-CELL                      ,  1 cells ,
   FRIEND-ARENA                   ,  FRIEND-ARENA-LEN ,
   CMBK-CELL                      ,  1 cells ,
   CMTAG-CELL                     ,  1 cells ,
   CMPADS-CELL                    ,  1 cells ,
   CMFRD-CELL                     ,  1 cells ,
   CMFR-OFF                       ,  CMFR-MAX cells ,
   CMFAM-CELL                     ,  1 cells ,
   BODYLEN-CELL                   ,  1 cells ,
   RBASE-CELL                     ,  1 cells ,
   LOOPSP-CELL                    ,  1 cells ,
   STACK-ABI:BASE-CELL            ,  1 cells ,
   SSCR-CELL                      ,  1 cells ,
   GTOD-SCRATCH                   ,  2 cells ,
   DOESP-CELL                     ,  1 cells ,
   VSP-CELL                       ,  1 cells ,
   VTAG-OFF                       ,  VSMAX ,
   CREATEP-CELL                   ,  1 cells ,
   QPATCH-CELL                    ,  1 cells ,
   QENT-CELL                      ,  1 cells ,
   QXH-CELL                       ,  1 cells ,
   VVAL-OFF                       ,  VSMAX cells ,
   NCOMP-DISPATCH:XT-CELL         ,  1 cells ,
   NCOMP-DISPATCH:DECL-CELL       ,  1 cells ,
   NCOMP-DISPATCH:TARGET-DECL-CELL ,  1 cells ,
   NCOMP-DISPATCH:TIER-CELL       ,  1 cells ,
   JIT-SNAP:SP-CELL               ,  1 cells ,
   NCOMP-DISPATCH:DEF-TIER-CELL   ,  3 cells ,
   LASTC-CELL                     ,  1 cells ,
   RSP-CELL                       ,  1 cells ,
   EXITH-CELL                     ,  1 cells ,
   LVD-CELL                       ,  1 cells ,
   GENIO-ABI:OUT-CELL             ,  GENIO-ABI:END GENIO-ABI:OUT-CELL - ,
   AOT-SPAN:TABLE-CELL            ,  3 cells ,
   SIGNAL-ABI:STUB-CELL           ,  3 cells ,
   FRAME-CELL                     ,  1 cells ,
   QFRAME-CELL                    ,  1 cells ,
   BODYBUF-OFF                    ,  BODYBUF-CAP 2 + ,
   RPKG-CUR                       ,  RPKG-REC RPKG-CUR - 1 cells + ,
   CMM-CELL                       ,  1 cells ,
   DOESB-CELL                     ,  1 cells ,
   TRUSTED-CELL                   ,  1 cells ,
   SRCLOC:PATH-CELL               ,  1 cells ,
   SRCLOC:PATHLEN-CELL            ,  1 cells ,
   SRCLOC:INB-CELL                ,  1 cells ,
   PKGRESYNC-CELL                 ,  1 cells ,
   HIDX:CLAIMS                    ,  1 cells ,
   PROT:WINDOW                    ,  1 cells ,
   PROT:WLO                       ,  1 cells ,
   PROT:RLO                       ,  1 cells ,
   ENGINE-HOOK-OFF                ,  ENGINE-HOOK-LEN ,
   LOCNAMES                       ,  LOC-RECS LOC-REC * ,
   REPLH-CELL                     ,  1 cells ,
   RSAVCP-CELL                    ,  1 cells ,
   RSAVND-CELL                    ,  1 cells ,
   RSAVDP-CELL                    ,  1 cells ,
   RSAVSP-CELL                    ,  1 cells ,
   RRECP-CELL                     ,  1 cells ,
   ARGC-CELL                      ,  1 cells ,
   ARGV-CELL                      ,  1 cells ,
   ENVP-CELL                      ,  1 cells ,
   PEND-CELL                      ,  1 cells ,
   TKA-CELL                       ,  1 cells ,
   TKL-CELL                       ,  1 cells ,
   INP-CELL                       ,  1 cells ,
   INE-CELL                       ,  1 cells ,
   FRCLM-CELL                     ,  1 cells ,
   BPA-CELL                       ,  1 cells ,
   BPTAB-OFF                      ,  EVALD-CELL BPTAB-OFF - ,
   EVALD-CELL                     ,  1 cells ,
   EVALERR-CELL                   ,  1 cells ,
   LMAINP-CELL                    ,  1 cells ,
   BPWBASE-CELL                   ,  1 cells ,
   BPWN-CELL                      ,  1 cells ,
   SNAP-CELL                      ,  1 cells ,
   NULL-PTR-CELL-OFF              ,  1 cells ,
   $3A00                          ,  $288 ,
   TASK-TCB-CELL                  ,  1 cells ,
   TASKS-LIVE-CELL                ,  1 cells ,
   HIDXP-CELL                     ,  1 cells ,
   EVALREC-CELL                   ,  1 cells ,
   AOT-SEED-DONE-CELL             ,  1 cells ,
   BOOT-SRC:USER-END              ,  1 cells ,
   PROT-REG-OFF                   ,  PROT-REG-LEN ,
   $40C8                          ,  $100 ,
   APP-ENTRY:XT-CELL              ,  1 cells ,
   AOT-WINDOW:T0-CELL             ,  1 cells ,
   AOT-WINDOW:D0-CELL             ,  1 cells ,
   AOT-WINDOW:B0-CELL             ,  1 cells ,
   EVAL-TOP-CELL                  ,  PROT:RHI EVAL-TOP-CELL - ,
   PROT:RHI                       ,  1 cells ,
   PROT:CF                        ,  1 cells ,
   AOT-SIG:POOL-CELL              ,  1 cells ,
   AOT-SIG:LEN-CELL               ,  1 cells ,
   BOOT-LAYOUT:HEAP-START-CELL    ,  1 cells ,
   STACK-ABI:CAP-CELL             ,  1 cells ,
   STACK-ABI:REPL-BASE-CELL       ,  1 cells ,
   STACK-ABI:REPL-CAP-CELL        ,  1 cells ,
   STACK-ABI:RETURN-BASE-CELL     ,  1 cells ,
   STACK-ABI:LOOP-BASE-CELL       ,  1 cells ,
   TXN-STATE-OFF                  ,  TXN-STATE-LEN ,
   USER-BAND:START                ,  USER-BAND:END USER-BAND:START - ,
   FS-ABI:START                   ,  FS-ABI:BYTES ,
   FMT-ABI:START                  ,  FMT-ABI:BYTES ,
   STRING-ABI:START               ,  STRING-ABI:BYTES ,
   PD-TABLE-OFF                   ,  PD-TABLE-END PD-TABLE-OFF - ,
   USE-BAND-OFF                   ,  USE-BAND-END USE-BAND-OFF - ,
   SNAP-RELOC:CALLMAP-OFF         ,  SNAP-RELOC:CALLMAP-BYTES ,
   SNAP-RELOC:ADDRMAP-OFF         ,  SNAP-RELOC:ADDRMAP-BYTES ,
   SNAP-RELOC:XTCELL-N-CELL       ,  SNAP-RELOC:XTCELL-END SNAP-RELOC:XTCELL-N-CELL - ,
   JIT-SNAP:STK-OFF               ,  JIT-SNAP:END JIT-SNAP:STK-OFF - ,
   TIER-PROV:OPEN-CELL            ,  TIER-PROV:END TIER-PROV:OPEN-CELL - ,
   0 ,  0 ,

: ROW-OFF ( n -- n )
   ROW-CELLS * cells TAB + @ ;

: ROW-LEN ( n -- n )
   ROW-CELLS * cells 1 cells + TAB + @ ;

: NAME-AT ( n -- ptr u8 n ) {: ix :}
   NAMES 0 begin dup ix < while
      swap dup c@ 1+ + swap 1+
   repeat drop dup 1+ swap c@ ;

: COUNT-ROWS ( -- n )
   0 begin dup ROW-LEN 0 <> while 1+ repeat ;

\ Half-open [off, off+len) intersection. A zero-length row cannot exist: the
\ walk stops on one, so a claim declared with no extent ends the table early
\ instead of being silently skipped - which is why every row states `1 cells`
\ rather than nothing.
: OVERLAP? ( n n -- bool ) {: a b :}
   a ROW-OFF b ROW-OFF b ROW-LEN + < 
   b ROW-OFF a ROW-OFF a ROW-LEN + < and ;

: CLAIMS-DIE ( n n -- ) {: a b :}
   MSG-RESET
   s" layout: DATA-CLAIMS overlap: " MSG+
   a NAME-AT MSG+
   s"  and " MSG+
   b NAME-AT MSG+
   MSG$ 76 die ;

: CLAIMS-ASSERT ( -- )
   COUNT-ROWS {: n :}
   0 begin dup n < while
      dup 1+ begin dup n < while
         2dup OVERLAP? if 2dup CLAIMS-DIE then
         1+
      repeat drop
      1+
   repeat drop ;

CLAIMS-ASSERT
;package
