//! Read-Eval-Print Loop for Habu
//!
//! Interactive REPL that ties together:
//! - Reader (parsing S-expressions)
//! - Compiler (S-expr → IR)
//! - Emitter (IR → bytecode)
//! - VM (bytecode execution)

const std = @import("std");
const reader = @import("../reader/reader.zig");
const Parser = reader.Parser;
const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;
const Env = compiler.Env;
const ir = @import("../compiler/ir.zig");
const IrBuilder = ir.IrBuilder;
const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Op = bytecode.Op;
const disasm = bytecode.disasm;
const vm_mod = @import("vm.zig");
const Vm = vm_mod.Vm;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const String = runtime.String;

pub const ReplError = error{
    ParseError,
    CompileError,
    EmitError,
    RuntimeError,
    IoError,
};

/// REPL configuration
pub const Config = struct {
    /// Show disassembly before execution
    show_disasm: bool = false,
    /// Show bytecode bytes
    show_bytes: bool = false,
    /// Prompt string
    prompt: []const u8 = "🐍 ",
    /// Continuation prompt (for multi-line input)
    cont_prompt: []const u8 = "   ",
};

/// REPL state
pub const Repl = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: Vm,
    config: Config,
    /// Persistent compiler for global definitions
    compiler: Compiler,
    /// Persistent chunk storage for closures (stored individually to avoid reallocation)
    persistent_chunk_ptrs: std.ArrayList(*bytecode.Chunk),

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, config: Config) Repl {
        return .{
            .allocator = allocator,
            .heap = heap,
            .vm = Vm.init(allocator, heap),
            .config = config,
            .compiler = Compiler.init(allocator),
            .persistent_chunk_ptrs = std.ArrayList(*bytecode.Chunk){},
        };
    }

    pub fn deinit(self: *Repl) void {
        self.compiler.deinit();
        for (self.persistent_chunk_ptrs.items) |chunk_ptr| {
            self.allocator.free(chunk_ptr.code);
            self.allocator.free(chunk_ptr.constants);
            self.allocator.destroy(chunk_ptr);
        }
        self.persistent_chunk_ptrs.deinit(self.allocator);
    }

    /// Run the REPL loop with File-based I/O
    pub fn runWithFiles(self: *Repl, stdin: std.fs.File, stdout: std.fs.File) !void {
        var line_buf: [4096]u8 = undefined;
        var out_buf: [4096]u8 = undefined;
        var out_writer = stdout.writer(&out_buf);
        const writer = &out_writer.interface;

        while (true) {
            // Print prompt
            try writer.writeAll(self.config.prompt);
            try writer.flush();

            // Read line manually
            var i: usize = 0;
            while (i < line_buf.len) {
                var byte_buf: [1]u8 = undefined;
                const n = stdin.read(&byte_buf) catch break;
                if (n == 0) return; // EOF
                if (byte_buf[0] == '\n') break;
                line_buf[i] = byte_buf[0];
                i += 1;
            }

            if (i == 0) continue;

            const line = line_buf[0..i];

            // Skip empty lines
            const trimmed = std.mem.trim(u8, line, " \t\r\n");
            if (trimmed.len == 0) continue;

            // Handle commands
            if (trimmed[0] == ',') {
                try self.handleCommand(trimmed, writer);
                continue;
            }

            // Eval and print
            self.evalPrint(trimmed, writer) catch |err| {
                try writer.print("Error: {s}\n", .{@errorName(err)});
            };
        }
    }

    /// Run the REPL loop (for testing with anytype readers)
    pub fn run(self: *Repl, in_reader: anytype, writer: anytype) !void {
        _ = self;
        _ = in_reader;
        _ = writer;
        // This version is for tests only - use runWithFiles for actual REPL
    }

    /// Evaluate a string and print the result
    pub fn evalPrint(self: *Repl, source: []const u8, writer: anytype) !void {
        const result = try self.eval(source);
        try self.printValue(result, writer);
        try writer.writeAll("\n");
    }

    /// Evaluate a string, return the result
    pub fn eval(self: *Repl, source: []const u8) !Value {
        // Use arena for IR nodes to simplify cleanup
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Parse
        var parser = Parser.init(arena_alloc, self.heap, source);
        defer parser.deinit();

        const expr = parser.parse() catch return error.ParseError;

        // Compile - use persistent compiler for globals, but temp builder
        // Save and restore the builder since it uses arena allocator
        const saved_builder = self.compiler.builder;
        self.compiler.builder = IrBuilder.init(arena_alloc);

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(expr, &env) catch |err| {
            self.compiler.builder = saved_builder;
            return if (err == error.UnboundVariable) error.CompileError else error.CompileError;
        };
        self.compiler.builder = saved_builder;

        // Emit bytecode
        var emitter = Emitter.init(self.allocator);

        emitter.emit(ir_node) catch {
            emitter.deinit();
            return error.EmitError;
        };
        const chunk = emitter.finalize() catch {
            emitter.deinit();
            return error.EmitError;
        };
        const child_chunks = emitter.getChildChunks() catch {
            self.allocator.free(chunk.code);
            self.allocator.free(chunk.constants);
            emitter.deinit();
            return error.EmitError;
        };
        emitter.deinit();

        defer self.allocator.free(chunk.code);
        defer self.allocator.free(chunk.constants);

        // Add child chunks to persistent storage (each allocated separately)
        // Store the base index for this eval's chunks
        const chunk_base = self.persistent_chunk_ptrs.items.len;
        for (child_chunks) |c| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
            chunk_ptr.* = c;
            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.free(c.code);
                self.allocator.free(c.constants);
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
        }
        self.allocator.free(child_chunks);

        // Optionally show disassembly
        if (self.config.show_disasm) {
            const stdout_file = std.fs.File.stdout();
            var buf: [4096]u8 = undefined;
            var file_writer = stdout_file.writer(&buf);
            const w = &file_writer.interface;
            disasm.disassemble(&chunk, w) catch {};
            w.writeAll("\n") catch {};
            w.flush() catch {};
        }

        // Set chunk pool - VM uses pointers from persistent storage
        // The make_closure indices are relative to this eval, so offset by chunk_base
        self.vm.setChunkPoolWithBase(self.persistent_chunk_ptrs.items, chunk_base);
        return self.vm.run(&chunk) catch return error.RuntimeError;
    }

    /// Print a value in Lisp notation
    pub fn printValue(self: *Repl, val: Value, writer: anytype) anyerror!void {
        if (val.isNil()) {
            try writer.writeAll("nil");
        } else if (val.eq(Value.t)) {
            try writer.writeAll("t");
        } else if (val.isFixnum()) {
            try writer.print("{d}", .{val.toFixnum()});
        } else if (val.isCons()) {
            try self.printList(val, writer);
        } else if (val.isSymbol()) {
            const sym = val.toPtr(Symbol);
            try writer.writeAll(sym.getName());
        } else if (val.isString()) {
            const str = val.toPtr(String);
            try writer.print("\"{s}\"", .{str.bytes()});
        } else if (val.isKeyword()) {
            try writer.writeAll(":<keyword>");
        } else if (val.isClosure()) {
            try writer.writeAll("#<closure>");
        } else if (val.isVector()) {
            try writer.writeAll("#<vector>");
        } else {
            try writer.print("#<unknown 0x{x}>", .{val.raw});
        }
    }

    fn printList(self: *Repl, val: Value, writer: anytype) anyerror!void {
        try writer.writeAll("(");

        var current = val;
        var first = true;

        while (current.isCons()) {
            if (!first) try writer.writeAll(" ");
            first = false;

            const cons = current.toPtr(Cons);
            try self.printValue(cons.car, writer);
            current = cons.cdr;
        }

        // Handle improper list
        if (!current.isNil()) {
            try writer.writeAll(" . ");
            try self.printValue(current, writer);
        }

        try writer.writeAll(")");
    }

    fn handleCommand(self: *Repl, cmd: []const u8, writer: anytype) !void {
        if (std.mem.eql(u8, cmd, ",q") or std.mem.eql(u8, cmd, ",quit")) {
            std.process.exit(0);
        } else if (std.mem.eql(u8, cmd, ",d") or std.mem.eql(u8, cmd, ",disasm")) {
            self.config.show_disasm = !self.config.show_disasm;
            try writer.print("Disassembly: {s}\n", .{if (self.config.show_disasm) "on" else "off"});
        } else if (std.mem.eql(u8, cmd, ",h") or std.mem.eql(u8, cmd, ",help")) {
            try writer.writeAll(
                \\Commands:
                \\  ,q ,quit    Exit REPL
                \\  ,d ,disasm  Toggle disassembly display
                \\  ,h ,help    Show this help
                \\
            );
        } else {
            try writer.print("Unknown command: {s}\n", .{cmd});
        }
    }
};

/// Convenience function to evaluate a string
pub fn evalString(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var repl = Repl.init(allocator, heap, .{});
    return repl.eval(source);
}

// ============================================================================
// Tests
// ============================================================================

test "eval fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "42");
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "nil");
    try testing.expect(result.isNil());
}

test "eval arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(+ 10 20)");
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "eval nested arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(+ (* 3 4) (- 10 5))");
    try testing.expectEqual(@as(i64, 17), result.toFixnum());
}

test "eval cons" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(car (cons 1 2))");
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "eval if true" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(if t 1 2)");
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "eval if false" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(if nil 1 2)");
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "eval comparison" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(< 5 10)");
    try testing.expect(result.eq(Value.t));

    const result2 = try evalString(allocator, &heap, "(> 5 10)");
    try testing.expect(result2.isNil());
}

test "eval type predicate" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(consp (cons 1 2))");
    try testing.expect(result.eq(Value.t));

    const result2 = try evalString(allocator, &heap, "(null nil)");
    try testing.expect(result2.eq(Value.t));
}
