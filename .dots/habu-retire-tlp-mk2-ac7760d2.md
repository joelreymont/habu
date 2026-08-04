---
title: Retire TLP-MK2/UN2/MK4/UN4 TRUSTED seed makers on TFAM 8/9
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:22:23.942547+02:00"
---

test/type-layout-lower-pending.f defines TLP-MK2/TLP-MK4 as checked generated-constructor callers and retains TRUSTED: TLP-UN2/TLP-UN4 to expose raw layout bundle cells for pass-2 value assertions. The unpackers are a named source-local boundary with this retirement owner and the focused type-layout-lower-pending production test. When MATCH provides checked payload refinement and tag dispatch, rewrite those assertions through MATCH and delete TLP-UN2/TLP-UN4. Acceptance: no TLP-MK*/UN* TRUSTED definition remains, the execution rows still prove whole-bundle transport preservation through checked constructors and destructuring, and no replacement ADT trust is introduced.

MK ROWS RETIRED (TFAM 11 slice 2, commit "Seed layout transport rows via generated ctors"): TLP-MK2/TLP-MK4 are checked definitions calling the generated constructors (7 TLP--RES:ERR -> payload 7 tag 1; 91 92 93 TLP--MIX:BIG -> payloads 91/92/93 tag 1). REMAINING: TLP-UN2/TLP-UN4 still expose bundle cells for value assertions. MATCH must replace that source-local boundary through checked branches before the unpackers can be deleted.
