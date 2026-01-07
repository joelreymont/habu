---
title: new
status: closed
priority: 2
issue-type: task
created-at: "2026-01-01T11:26:22.960336+02:00"
closed-at: "2026-01-01T12:01:52.831133+02:00"
close-reason: "Implemented open (0xCD) and close (0xCE) opcodes in src/bytecode/opcodes.zig and src/interp/vm.zig. VM implementation complete using std.fs.File. Supports :input/:output. Lisp-level bindings (primitive functions in compiler) still needed for calling from Lisp code."
---

Implement (open pathname &key direction element-type if-exists if-does-not-exist external-format). Add open opcode to opcodes.zig (0xC0), implement in vm.zig using std.fs.File.openFile(). Store file descriptor in Stream.file_fd. Direction: :input or :output. Return Stream object with type=.file. Location: src/interp/vm.zig:~2900, src/bytecode/opcodes.zig:~860
