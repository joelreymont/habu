---
title: Reduce directory listing overhead
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T22:16:08.631101+03:00"
---

Otto measured dot show filesystem overhead: /Users/joel/.cache/tmp/dots-show-habu-repro perunit.py list/list-probe vs list/list-c, empty listing210.0K instructions vsplainC192.0K orCwithmapping206.5K; NEXT/RECORD/SLOT around500instructions/entry vs10C. FS-LIST currently maps/unmaps4KiB perlisting toensurecaller/task/reentrantownership. Reduce measured decoding/state indirection and provide responsible owner/caller storage reuse if it removes mapping cost without sharedglobalstate, leaks, recursion races or API workarounds. Coordinate alreadyowned typedentries+errno lane; do not create pools/framework or remove distinct failure/cleanup boundaries. Existing real nested/concurrent/callbackfailure tests first; same-source empty andpopulated listed outputs/counts, instructionmeasurement withmap/plain controls, independent Astra review and proportionalgates. Dots ownscaller migration, Habu owns generic API.
