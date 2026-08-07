---
title: native-reach assert 41 is red on master
status: active
priority: 1
issue-type: task
created-at: "2026-08-07T11:27:02.978232+02:00"
---

test/compiler/native-reach.f CLOBBER-CASES, the assertion 'the refused move left every site where it found it', fails on master: 'NRT-BASE:NRT-CL-CALLER' 'NRT-NARROW:NRT-CL' CODEGEN-SCAN:CALLS-IN answers 0 where the case expects 2. Measured 2026-08-07 on a clean workspace at master 1c664f2c AND at 77115b79, with no local changes - so it is not the name-resolution work in habu-complete-the-chain-5aab8cee, which was checked by disabling RESOLVE-SCAN and observing the same failure. The case is the guard on NREACH:REDIRECT's refusal path: it asserts that a move refused for clobber-widening left both call sites in the caller untouched. CALLS-IN answering 0 means the scan finds no call to the narrow routine in NRT-BASE:NRT-CL-CALLER at all, so the assertion cannot distinguish 'the refused move left the sites alone' from 'there were never any sites' - the case has stopped doing its work whether or not REDIRECT is correct. Find whether the caller stopped being compiled with calls (inlining, a changed entry address, a migration that now succeeds where it used to be refused) or whether CODEGEN-SCAN:CALLS-IN stopped matching, and restore the case so it fails when REDIRECT writes to a refused site.

Claim: agent=reachfix workspace=.jj-ws/habu-native-reach-assert-a148e90c
