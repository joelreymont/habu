---
title: Recompile only the files that use a changed word
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:47:01.501718+02:00"
---

Problem: the linker (src/habu/linker.f) tracks one time per file piece (COMPILED-TIME :2805, FILES rows :160-167), so any change gives the piece a new time and every file that names one of its words (NAMED-CK :2106), refers to its records (ROWS-CK :2100) or ran a word after it stored into header data (WALK-STORERS :888, REACHED-CK :2120) recompiles. Measured 2026-11-10 on master 7eb193e5 with a probe engine (the probe workspace and its logs are retired): one unused word appended to src/habu/repl.f compiles 40 files, 39 to the bytes they had (8 by name, 31 by repl.f's store into REPLH-CELL); one appended to src/lib/fmt.f compiles 29, 27 to the bytes they had; a comment compiles 1. Joel 2026-11-10: 'didn't we take great care to figure out if any words have been recompiled and then only recompile packages that use those words?', 'what happened to figuring out what words have changed?'. Acceptance: each of the two one-word edits recompiles the edited file only; a body or effect change recompiles the files that use that word; warm equals cold before the signature; the engine builds itself to a fixpoint. Files: src/habu/linker.f and what the design names. Verify: the three edits with the built hb, cmp cold and warm. Depends: the design (brief .jj-ws.noindex/carl-layout/.out/briefs/design-word-level-dependencies.md, Plan agent running 2026-11-10); the design goes to Joel before a worker starts. Ownership: carl. Claim: unassigned.

Joel 2026-11-10, shown three rules for a number baked in from the data layout (dot habu-rebuild-a-file-b5f9a929, moved here from jerry; his findings /tmp/jerry-f1-findings.md): "you are fixing the symptom and not the cause, prove me wrong! the cause here is that we had code that checked whether a word has changed and identified the consumers down the line to see what it should recompile!", then "should we not restore word tracking and check if it 1) does not have a significant time impact and 2) check it fixes the issues?" Added acceptance: (1) cold and warm no-edit engine build times with word tracking against today's (cold 13.4 s, warm 1.8 s); (2) b5f9a929's reproducer, direct and hidden form (`: OFF ( -- n ) here data-base - ; OFF constant M`), builds warm to what a cold build gives. What that file consumed is where data lies, read through hb's own words; it names no changed word, so the record of what a file consumed has to hold that too. Measured 2026-11-10 (same probe engine; logs retired): `create PROBE-PAD 64 allot` appended to src/core/util.f, the first file, then every source touched so all 328 pieces compile: 326 compile to the bytes they had; the same with 61 for 64. No engine file's result depends on where earlier data lies, so a rule that recompiles every later file after data moves costs a cold build per edit and changes no byte of hb. Not measured: files outside the engine build (ten library lines spell alignment as `here data-base - negate 7 and allot`), and whether a file's data start is aligned.

Joel 2026-11-10, shown the cut design: "does this work? that's the ultimate test! also, you need to determine the undetermined". The test of "works" (lead): in an early widely used file (src/core/util.f) and a late one (src/habu/repl.f), each of six edits recompiles the edited file and only files that use what changed, and the unsigned warm program equals a cold one: a comment; a new unused word at the end; a new unused word mid-file; a body edit of one word; a string literal made longer mid-file; a new variable mid-file. Known gap in the design as written: a data reference is an offset into the whole file's data (linker.f DATA-REF+ :911, tag 3), so the last two edits change the bytes of every word that refers to later data of the file, and of their users. Design, census and decisions: .jj-ws.noindex/carl-layout/.out/design-word-level.md. Lanes: habu-drop-header-stores-02065287 (carl-hdr), habu-give-each-word-26c91f8b (carl-wt). Both lanes stacked on master 8fc5961a in .jj-ws.noindex/carl-words (xupuznmx word times, xwwymlnw header stores), where the feature is built, reviewed and landed whole.

Six-edit measurement 2026-11-10 at the stack tip (zolomuqz da10c376 on mvlkqltv; engine build, objects rewritten by the warm build over a cold base; scratch ~/.cache/tmp/carl-words/six, run1 and run2, lists in */*.files). Base cold 13.2 s. util.f has two objects (kernel and engine load), counted as written.

| edit | src/core/util.f | warm | src/habu/repl.f | warm |
|---|---|---|---|---|
| comment | 2 (util.f twice) | 1.7 s | 1 | 1.6 s |
| unused word at end | 202 | 13.5 s | 11 | 2.4 s |
| unused word mid-file | 213 | 12.2 s | 13 | 2.4 s |
| body edit (`here drop` added) | 204 | 12.1 s | 11 | 2.5 s |
| string made longer mid-file | 202 | 12.0 s | 11 | 2.4 s |
| new variable mid-file | 213 | 12.3 s | 13 | 2.4 s |

Every warm program equals its cold one unsigned (12 of 12). The first run's body edit was `0 drop`, which compiles to the base's bytes and rewrote only the edited file: the script now prints whether the program differs from the base's, and the body row above is the rerun. Every edit that gives the piece a new time costs the same, whatever changed: the files that list the piece whole. For repl.f the 10 besides itself all list it in REACHED (the run rule, by piece); mid-file edits add main.f and runtime.f. Next lane: the run rule per word.

Six-edit rerun 2026-11-10 at the integration tip 0fbd7181 (word references kept, the run rule by word, header stores by mark; scratch ~/.cache/tmp/carl-words/six/run3, lists in run3/*.files). Base cold 12.7 s. Workers' builds ran beside it, so wall times are upper bounds.

| edit | src/core/util.f | warm | src/habu/repl.f | warm |
|---|---|---|---|---|
| comment | 2 (util.f twice) | 1.8 s | 1 | 1.9 s |
| unused word at end | 13 | 4.7 s | 3 | 1.9 s |
| unused word mid-file | 166 | 11.4 s | 5 | 1.9 s |
| body edit (`here drop` added) | 142 | 15.1 s | 3 | 1.9 s |
| string made longer mid-file | 13 | 4.9 s | 3 | 3.7 s |
| new variable mid-file | 166 | 11.5 s | 5 | 2.5 s |

Every warm program equals its cold one unsigned (12 of 12). What still recompiles: (a) an edit that only gives the piece a new time (end, string) rewrites the files that list the piece whole and reached: for util.f its two objects and 11 more (boot.f, kernel-object.f, link-kernel.f, compiler.f, check-hook.f twice, checker.f twice, link-data.f, linker.f, end.f), for repl.f itself, stepper.f and end.f: the run rule reaching the piece's data, whole. (b) a word or variable added mid-file rewrites 166 for util.f and adds main.f and runtime.f for repl.f: a reference names a record by its index in the file, which the addition shifts. (c) util.f's body edit is in CORE-FOLD-C, which 142 objects reach.

Class (c) checked 2026-11-10 against run3 (scripts ~/.cache/tmp/carl-words/an/words.py and reqs.py over run3/base and run3/util-body.files). The 142 objects util.f's body edit rewrote are: util.f's own 2; 126 that list CORE-FOLD-C (util.f word 4) in WORDS, and every object that lists it was rewritten (126 of 126); 14 that list no CORE-FOLD-C row and each request a rewritten file (the requester chain, dot habu-a-requester-compiles-784d47ff). Nothing recompiled outside the rule. CORE-FOLD-C is spelled in three other files (arena.f, render.f, type-family.f); the other objects list it because a word they ran reaches it through CORE-STR=CI, which is the run rule working as designed.

Six-edit rerun 2026-11-10 at the stack tip f015211f (records, entries, facts rows and quotations named by identity; scratch ~/.cache/tmp/carl-words/six/run4, lists in run4/*.files). Base cold 14.7 s wall with a worker's build beside it, so wall times are upper bounds.

| edit | src/core/util.f | warm | src/habu/repl.f | warm |
|---|---|---|---|---|
| comment | 2 (util.f twice) | 2.1 s | 1 | 2.0 s |
| unused word at end | 13 | 5.0 s | 3 | 2.0 s |
| unused word mid-file | 13 | 6.3 s | 3 | 2.1 s |
| body edit (`here drop` added) | 142 | 12.3 s | 3 | 2.1 s |
| string made longer mid-file | 13 | 4.8 s | 3 | 2.2 s |
| new variable mid-file | 19 | 5.1 s | 3 | 2.1 s |

Every warm program equals its cold one unsigned (12 of 12). Mid-file insertion now costs what an end insertion does (166 to 13 and 5 to 3). What still recompiles: the 11 files besides util.f's two objects for every edit that gives the piece a new time (boot.f, kernel-object.f, link-kernel.f, compiler.f, check-hook.f twice, checker.f twice, link-data.f, linker.f, end.f), and stepper.f and end.f for repl.f; the variable edit adds definition.f, interpret.f, link-map.f, outer.f and error.f twice. Next: data named by its owning word (dot habu-name-a-file-1ce8f6b8).

Six-edit rerun 2026-11-10 at a0946130 (a part per object; scratch ~/.cache/tmp/carl-words/six/run5, lists in run5/*.files, decoder ~/.cache/tmp/carl-words/an/words5.py). Base cold 12.6 s. An object is now a part (496 objects for 328 files), so the counts are of parts and compare with run4 only by file.

| edit | src/core/util.f: parts (files) | warm | src/habu/repl.f | warm |
|---|---|---|---|---|
| comment | 2 (util.f in both loads) | 1.7 s | 1 | 1.7 s |
| unused word at end | 34 (9) | 4.5 s | 3 | 1.8 s |
| unused word mid-file | 34 (9) | 4.5 s | 3 | 1.7 s |
| body edit (`here drop` added) | 150 (96) | 10.2 s | 3 | 1.8 s |
| string made longer mid-file | 34 (9) | 4.6 s | 3 | 2.0 s |
| new variable mid-file | 39 (13) | 4.7 s | 3 | 3.4 s |

Every warm program equals its cold one unsigned (12 of 12). By file against run4: kernel-object.f no longer compiles for any util.f edit (it only requested boot.f); interpret.f no longer compiles for the variable edit; the body edit lost 12 files (108 to 96), the requester chain. Every part beyond the edited file, read from the objects:
- end, mid, string (34): util.f twice; 12 parts that hold util.f whole and in REACHED (boot.f+1 and +4, link-kernel.f+1, compiler.f+12 and +13, check-hook.f twice, checker.f twice, link-data.f+6, linker.f+3, end.f); 20 parts that follow one of those in its file and compile because a compile runs to the file's end (boot.f+2, +3, +5 to +17, link-kernel.f+2, link-data.f+7 to +10). The data lanes (habu-name-a-file-1ce8f6b8) remove the 12 and so the 20.
- variable (39): those 34 and definition.f, link-map.f, outer.f+1 and error.f twice, whose words name data laid after the new variable by offset. The data lanes remove them.
- body (150): util.f twice; 130 parts that list CORE-FOLD-C (sum 4781309076379262046) in WORDS, and every part that lists it was rewritten (130 of 130); 18 parts that follow a compiled part in boot.f and link-data.f. Nothing compiled outside the rule. The data lanes do not aim at this row.
- repl.f (3): repl.f, and stepper.f and end.f, which hold it whole and in REACHED.

fmt.f check (chain.sh, ~/.cache/tmp/carl-words/chain/run5): a comment appended to src/lib/fmt.f rewrites 5 objects, all fmt.f's (two parts in the engine load, three in the kernel load), user 1.47 s; an unused word appended rewrites the same 5.

Rebase 2026-11-10 (carl): the installed hb, rebuilt from master at 16:36, no longer loads the stack's sources (E-LIFECYCLE-LATE left src/lib/error.f with master's "Delete image-lifecycle"), so the stack moved onto master bdeb5574 (27 commits with data lane 1's; tip bf9c6f8f). Master's four pending-piece commits (edf805b9, df8d5c21, 7ffd2750, 7eb0ee10) extend the request check that "Link and compile a file part by part" deletes: commits before it carry master's pending words merged by hand and were not built; that commit takes the stack's side; "Remove what the part machine left" deletes the rest (PENDING-RECS, -FIRST, -OWN, -SLOTS, -HITS, -STORERS and their readers). Kept from master: E-LIVE-TASK and OPEN-TASKS, SAVED-EXIT-HOOK and ARM-EXIT, RESET-SCRATCH. test/file-objects.f keeps master's cases (stale, hidden, sealed, stored, retired, order, scratch, task-left, word-load, kept) with FILES-LEN-FIELD 64 from the part header on. Evidence at bf9c6f8f, ev.sh (~/.cache/tmp/carl-words/e-rebase.out): hb1-hb5 rc 0, warm rewritten 0, cold = warm unsigned, probe rewritten 3, restored = cold, file-objects passes (61 s). Not rerun on the rebased tip: six.sh, chain.sh, bob's flip reproducer, the full suite.

Six-edit rerun 2026-11-10 at the stack tip rxpunmmt 2d5cdbe6 "Reach data by extent" (lane 4a; scratch ~/.cache/tmp/carl-words/six/run7.out, lists in run7/*.files). Base cold 13.1 s; cold builds 12.6 to 13.5 s. Objects rewritten by the warm build:

| edit | src/core/util.f | warm | src/habu/repl.f | warm |
|---|---|---|---|---|
| comment | 2 (util.f twice) | 1.8 s | 1 | 1.8 s |
| unused word at end | 2 | 1.7 s | 1 | 1.7 s |
| unused word mid-file | 2 | 1.8 s | 1 | 1.7 s |
| body edit (`here drop` added) | 146 | 10.2 s | 3 | 1.8 s |
| string made longer mid-file | 31 | 4.7 s | 3 | 1.8 s |
| new variable mid-file | 2 | 1.8 s | 1 | 1.8 s |

Every warm program equals its cold one unsigned (12 of 12). Comment, end, mid and variable edits compile the edited file only. Not yet so: the body and string edits. Of the string edit's 31, an older decoder shows 15 parts with no row for util.f (boot.f +3, +5 to +7, +9 to +17, link-data.f +8, +9): parts after a compiled part of their own file, which LOAD compiles to the file's end (linker.f :4790). Which row compiles the rest is being measured (child dot "Find which row compiles each part after a body or string edit", workspace carl-d6). Not run on this tip: the fmt.f chain, the full suite, chain-run-build.

Census 2026-11-10 at 2d5cdbe6 (child dot habu-find-which-row-531a9881): after the body and string edits every rewritten part either lists a word whose time changed (it ran or names it) or follows a compiled part of its own file (18 of 146, 20 of 31). Lead's conclusion: lane 4b (a part's changes time) answers nothing these edits measure and is not briefed; the lane left for the six-edit test is to go on linking a file's parts after one of them compiles (LOAD, linker.f:4790), not designed yet. Which ran word reaches CORE-FOLD-C in 120 parts is not determined.


Merged from habu-rebuild-a-file-b5f9a929 (Rebuild a file that read the data layout when earlier data grows), 2026-11-10:

Problem: a number a file takes from the data layout at top level is baked into its object, which then links after an earlier file's data grows: warm != cold. Reproducer (/tmp/jerry-inc/fx/q, remake): s.f 'create A-BUF 8 allot', f.f 'here data-base - constant M', r.f 'require s.f / require f.f / : MAIN ( -- ) M . cr ;'; build, change 8 to 64, build warm and cold (fresh HABU_OUT): warm prints 1099515828264, cold 1099515828320. f.f ran only words below FIRST-REC, which src/habu/linker.f RAN-RECORD ignores, and names nothing of s.f, so no FILES row ties it to s.f. The installed hb (d061bcee36) does the same; closed dot habu-find-the-layout-a39a3ad5 fixed three engine files of this class and left no rule. Acceptance: the reproducer's warm build prints what its cold build prints, by a dependency rule that holds for every file that reads the layout at top level; a case of it in test/file-objects.f. Until then 'warm = cold, every edit kind' has this counter-example and every such claim says so. Files: src/habu/linker.f; test/fixtures/file-objects/build.f; test/file-objects.f. Verify: the reproducer warm = cold; test/file-objects.f. Depends: a design of the rule, approved before a worker starts. Ownership: unassigned. Claim: unassigned.

Related (jerry, 2026-11-10): DECISIONS.md "linking a file's object" leaves "loader state at a file's start" to later steps, and this is that case: the value read is the layout pointer, which no store row or reference names. Dot habu-compile-a-file-a61e0d05 (a cell an earlier file changed) and its design, .jj-ws.noindex/carl-rowdep/.out/design-stale-read.md, cover changed cells and list this under "not covered" (§10). The same failure needs no `data-base`: `here A-BUF - constant M` after a file between the two grows. No list of words catches it; the number comes from a difference of addresses in two pieces. One mechanism is at hand once dot habu-keep-the-times-9a6159f6 lands: a file that never links costs only its own compile, and the files that depend on it link while its result is equal, so a file found to read the layout could simply never link. How a build finds such a file (the checker at top level, a fact per word the run log reaches) is the open design.

Checked (jerry, 2026-11-10, hb staged from 88e80a2f, fixture /tmp/jerry-inc/fx/pi): an immediate declared with parse-imm whose callee in another file changes is not this gap: the file whose body ran it compiles (definers.f DEF-RUN logs it), warm prints what cold prints and the programs are equal before the signature.

Handed to carl with the recompile-set work (jerry, 2026-11-10): Joel wrote "carl is looking into recompilation so send any findings to him, you should not be working on this", and of a rule that compiles every later file once data moved, "you are fixing the symptom and not the cause ... the cause here is that we had code that checked whether a word has changed and identified the consumers down the line to see what it should recompile". No rule is chosen and nothing was built. A design by a Plan agent (read only, never run; it misses a subtraction inside a word the top level runs) is in .jj-ws.noindex/jerry-wasm/.out/f1-design.md; the findings sent to carl are in /tmp/jerry-f1-findings.md. Claim: unassigned.

2026-11-10 (carl): design at carl-layout/.out/design-layout-number.md (Plan agent, cut by the lead; citations checked against carl-words). Part 1, the subtraction at a file's top level, found exactly from its two operands, is ready once Joel answers what a build does with such a file (never link, or link while the positions are unchanged). Part 2, the subtraction inside a run word, reads the per-word reach list of the run rule and waits for it; its first evidence is the count of engine files it would catch.

2026-11-10 (carl): Joel rejected the question on this design ("what the hell is this about and why is it needed? why is it a problem you decided to solve?"). Parked: no worker, no further design, until word tracking is finished and measured. No engine file is such a consumer; every "warm = cold" claim names this case as the exception.

Joel, 2026-11-10: "you need to finish incremental builds for release and make them rock-solid!"

Landing checks 2026-11-11 on the stack's final tip (logs ~/.cache/tmp/carl-d6/land/r2): engine built twice, 13 s each; chain check fixpoint at generation 2; suite 273 of 273. Measure, cold then warm, warm equal to cold with 0 bytes differing: Maki image 21 s and 3 s (tim's viewer-line workspace) and 22 s and 3 s (Maki master a6a916be), Maki viewer 4 s and 3 s (viewer-line; Maki master has no viewer-main.f), Zen server 13 s and 2 s (master ce4593c5), Tender server 20 s and 2 s (main 6279cdba), tender.f loads rc 0. Engine build 13.0 s cold, 1.8 s warm, 0 objects rewritten. The six edits, each warm equal to cold unsigned: src/core/util.f comment, end, mid and variable rewrite util.f's two objects (1.8 s); string 11 (4.4 s); body 135 (10.4 s). src/habu/repl.f comment, end, mid and variable rewrite repl.f alone (1.8 s); body and string 3 (repl.f, stepper.f, end.f; 1.9 s). Census of the body and string edits (~/.cache/tmp/carl-d6/census.py over r2/census): every rewritten object is the edited file's own or lists a word whose time changed (util body 133, util string 9, repl 2 and 2); followers 0; no kept object lists a changed word. A comment or an unused word appended to src/lib/fmt.f rewrites fmt.f's five objects only. Same-name cases v3 and v4: warm equals cold both ways. Open under this dot: habu-a-link-s-1b847f18.

Six-edit run8 (carl, 2026-11-11, master 63be288d with the installed hb, ~/.cache/tmp/carl-words/six/run8.out; other work ran on the machine, so walls are not comparable to run7 below 0.2 s): warm = cold unsigned in all 12. Objects rewritten and warm wall, run7 (the stack before it landed) then run8: util.f comment 2 and 2 (1.8 s, 1.8 s), end 2 and 2 (1.7, 1.9), mid 2 and 2 (1.8, 1.9), variable 2 and 2 (1.8, 1.8), body 146 and 135 (10.2, 11.7), string 31 and 11 (4.7, 4.4); repl.f comment, end, mid, variable 1 each and body, string 3 each, 1.7 to 1.9 s in both; cold 12.6 to 13.5 s in both. Not explained: util.f body compiles 11 fewer objects and took 1.5 s longer; the wall was not measured on a quiet machine.
