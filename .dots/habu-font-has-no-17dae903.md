---
title: FONT has no test that runs on Linux
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:13:22.964051+01:00"
---

Problem: test/font-native.f, FONT's one native test, reads macOS fonts (/System/Library/Fonts/Supplemental/Arial.ttf :16, Baskerville :72, STIXGeneral :81, Apple Color Emoji :93) and is in no suite, so on Linux it stops at E-FS-STAT before shaping anything. Maki shapes text through FONT on spark, where the system libharfbuzz.so.0 is 8.3.0, below font.f's 14.2+ (font.f:1-2): Maki's replay died on E-FFI-DLSYM hb_face_create_or_fail and no Habu test saw it. Acceptance: font-native.f's fonts are switched by platform, Linux using fonts from Ubuntu's packages, and it passes in the suite on spark with HarfBuzz 14.2+ on the loader path. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fontlinux.
