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
const diagnostic = @import("../diagnostic.zig");
const lineedit = @import("lineedit.zig");
const LineEditor = lineedit.LineEditor;

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
    /// Macro definitions: name -> closure
    macros: std.StringHashMap(Value),
    /// Line editor for interactive input
    line_editor: LineEditor,
    /// Current VM being used (for nested loads)
    current_vm: ?*Vm,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, config: Config) Repl {
        return Repl{
            .allocator = allocator,
            .heap = heap,
            .vm = Vm.init(allocator, heap),
            .config = config,
            .compiler = Compiler.initWithHeap(allocator, heap),
            .persistent_chunk_ptrs = std.ArrayList(*bytecode.Chunk){},
            .macros = std.StringHashMap(Value).init(allocator),
            .line_editor = LineEditor.init(allocator),
            .current_vm = null,
        };
    }

    /// Wire up VM to compiler's global environment. Must be called after init.
    pub fn wireGlobalEnv(self: *Repl) void {
        self.vm.setGlobalEnv(&self.compiler.globals);
        // Set up load callback
        self.vm.setLoadCallback(&loadCallback, @ptrCast(self));
        // Set up eval callback
        self.vm.setEvalCallback(&evalCallback, @ptrCast(self));
        // Set up macroexpand callback
        self.vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));
    }

    /// Callback for (load "filename") from VM
    fn loadCallback(filename: []const u8, context: *anyopaque) vm_mod.VmError!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.loadFileValue(filename) catch {
            return vm_mod.VmError.InvalidArgument;
        };
    }

    /// Callback for (eval expr) from VM
    fn evalCallback(expr: Value, context: *anyopaque) vm_mod.VmError!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.evalExpression(expr) catch {
            return vm_mod.VmError.InvalidArgument;
        };
    }

    /// Callback for (macroexpand expr) from VM
    fn macroexpandCallback(expr: Value, context: *anyopaque) vm_mod.VmError!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.expandMacros(expr) catch {
            return vm_mod.VmError.InvalidArgument;
        };
    }

    /// Evaluate an expression using a separate VM
    fn evalExpression(self: *Repl, expr: Value) !Value {
        // Use arena for compilation
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Save and set compiler state
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = ir.IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(expr, &env) catch {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            return error.CompileError;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
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

        // Store child chunks for closures
        const chunk_base: u16 = @intCast(self.persistent_chunk_ptrs.items.len);
        for (child_chunks) |c| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
            chunk_ptr.* = c;
            patchMakeClosureIndices(chunk_ptr.code, chunk_base);
            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.free(c.code);
                self.allocator.free(c.constants);
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
        }
        self.allocator.free(child_chunks);
        patchMakeClosureIndices(chunk.code, chunk_base);

        // Use a separate VM to avoid stack issues
        var nested_vm = Vm.init(self.allocator, self.heap);
        nested_vm.setGlobalEnv(&self.compiler.globals);
        nested_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        nested_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        nested_vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));

        // Copy globals from main VM
        for (self.vm.globals, 0..) |g, i| {
            nested_vm.globals[i] = g;
        }
        nested_vm.num_globals = self.vm.num_globals;

        // Set up chunk pool for closures
        nested_vm.setChunkPool(self.persistent_chunk_ptrs.items);

        // Execute
        const result = nested_vm.run(&chunk) catch {
            return error.RuntimeError;
        };

        // Copy back any new globals
        for (nested_vm.globals, 0..) |g, i| {
            self.vm.globals[i] = g;
        }
        if (nested_vm.num_globals > self.vm.num_globals) {
            self.vm.num_globals = nested_vm.num_globals;
        }

        return result;
    }

    /// Load a file and return the last value (for (load ...) primitive)
    /// Uses a separate VM to avoid recursive execution issues
    fn loadFileValue(self: *Repl, path: []const u8) !Value {
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return error.IoError;
        };
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch {
            return error.IoError;
        };
        defer self.allocator.free(content);

        // Evaluate all expressions using a fresh VM, return last value
        return self.evalFileContentSeparateVm(content);
    }

    /// Evaluate file content using a separate VM to avoid stack corruption
    fn evalFileContentSeparateVm(self: *Repl, content: []const u8) !Value {
        var pos: usize = 0;
        var last_value = Value.nil;

        // Create a temporary VM for nested evaluation
        var nested_vm = Vm.init(self.allocator, self.heap);
        nested_vm.setGlobalEnv(&self.compiler.globals);
        nested_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        nested_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        nested_vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));

        // Copy globals from current VM context (for nested loads)
        const source_vm = self.current_vm orelse &self.vm;
        for (source_vm.globals, 0..) |g, i| {
            nested_vm.globals[i] = g;
        }
        nested_vm.num_globals = source_vm.num_globals;

        // Save previous current_vm and set nested_vm as current
        const saved_current_vm = self.current_vm;
        self.current_vm = &nested_vm;
        defer self.current_vm = saved_current_vm;

        while (pos < content.len) {
            // Skip whitespace and comments
            while (pos < content.len) {
                if (content[pos] == ' ' or content[pos] == '\t' or
                    content[pos] == '\n' or content[pos] == '\r')
                {
                    pos += 1;
                } else if (content[pos] == ';') {
                    while (pos < content.len and content[pos] != '\n') {
                        pos += 1;
                    }
                } else {
                    break;
                }
            }

            if (pos >= content.len) break;

            // Find the expression extent
            const expr_start = pos;
            var depth: usize = 0;
            var in_string = false;
            var in_char = false;

            while (pos < content.len) {
                const c = content[pos];
                if (in_string) {
                    if (c == '\\' and pos + 1 < content.len) {
                        pos += 2;
                        continue;
                    }
                    if (c == '"') in_string = false;
                } else if (in_char) {
                    in_char = false;
                } else {
                    if (c == '"') in_string = true;
                    if (c == '#' and pos + 1 < content.len and content[pos + 1] == '\\') {
                        pos += 1;
                        in_char = true;
                    }
                    if (c == '(') depth += 1;
                    if (c == ')') {
                        if (depth > 0) depth -= 1;
                        if (depth == 0) {
                            pos += 1;
                            break;
                        }
                    }
                    if (depth == 0 and (c == ' ' or c == '\t' or c == '\n' or c == '\r')) {
                        break;
                    }
                }
                pos += 1;
            }

            const expr_slice = content[expr_start..pos];
            if (expr_slice.len > 0) {
                // Use evalWithVm for the nested VM
                last_value = self.evalWithVm(expr_slice, &nested_vm) catch {
                    continue;
                };
            }
        }

        // Copy globals back to source VM
        for (nested_vm.globals, 0..) |g, i| {
            source_vm.globals[i] = g;
        }
        source_vm.num_globals = nested_vm.num_globals;

        return last_value;
    }

    /// Evaluate with a specific VM instance
    fn evalWithVm(self: *Repl, source: []const u8, vm: *Vm) !Value {
        // Use arena for IR nodes
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Parse
        var parser = Parser.init(arena_alloc, self.heap, source);
        var expr = parser.parse() catch {
            return error.ParseError;
        };

        // Check for defmacro - handle specially like main eval
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Expand macros
        expr = self.expandMacros(expr) catch return error.CompileError;

        // Compile
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(expr, &env) catch {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            return error.CompileError;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
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
            return error.OutOfMemory;
        };

        // Store chunks persistently and patch indices
        const chunk_base: u16 = @intCast(self.persistent_chunk_ptrs.items.len);
        for (child_chunks) |child_chunk| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(chunk.code);
                self.allocator.free(chunk.constants);
                return error.OutOfMemory;
            };
            chunk_ptr.* = child_chunk;
            patchMakeClosureIndices(chunk_ptr.code, chunk_base);
            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(chunk.code);
                self.allocator.free(chunk.constants);
                return error.OutOfMemory;
            };
        }
        self.allocator.free(child_chunks);
        patchMakeClosureIndices(chunk.code, chunk_base);
        emitter.deinit();

        // Set chunk pool and run
        vm.setChunkPool(self.persistent_chunk_ptrs.items);
        const result = vm.run(&chunk) catch {
            self.allocator.free(chunk.code);
            self.allocator.free(chunk.constants);
            return error.RuntimeError;
        };
        self.allocator.free(chunk.code);
        self.allocator.free(chunk.constants);
        return result;
    }

    pub fn deinit(self: *Repl) void {
        self.line_editor.deinit();
        self.compiler.deinit();
        for (self.persistent_chunk_ptrs.items) |chunk_ptr| {
            self.allocator.free(chunk_ptr.code);
            self.allocator.free(chunk_ptr.constants);
            self.allocator.destroy(chunk_ptr);
        }
        self.persistent_chunk_ptrs.deinit(self.allocator);
        self.macros.deinit();
    }

    /// Run the REPL loop with File-based I/O
    pub fn runWithFiles(self: *Repl, stdin: std.fs.File, stdout: std.fs.File) !void {
        _ = stdin; // Line editor reads directly from stdin

        var out_buf: [4096]u8 = undefined;
        var out_writer = stdout.writer(&out_buf);
        const writer = &out_writer.interface;

        // Input accumulator for multi-line expressions
        var input_buf = std.ArrayList(u8){};
        defer input_buf.deinit(self.allocator);

        while (true) {
            // Get appropriate prompt
            const prompt = if (input_buf.items.len == 0) self.config.prompt else self.config.cont_prompt;

            // Read line with editing
            const line = self.line_editor.readline(prompt) catch {
                // Error reading - try to eval what we have and exit
                if (input_buf.items.len > 0) {
                    self.evalPrint(input_buf.items, writer) catch {};
                }
                return;
            } orelse {
                // EOF (Ctrl-D on empty line)
                if (input_buf.items.len > 0) {
                    self.evalPrint(input_buf.items, writer) catch {};
                }
                return;
            };

            const trimmed = std.mem.trim(u8, line, " \t\r\n");

            // Empty line on fresh input: skip
            if (trimmed.len == 0 and input_buf.items.len == 0) continue;

            // Handle commands only on fresh input
            if (input_buf.items.len == 0 and trimmed.len > 0 and trimmed[0] == ',') {
                try self.handleCommand(trimmed, writer);
                try writer.flush();
                continue;
            }

            // Accumulate input
            if (input_buf.items.len > 0) {
                try input_buf.append(self.allocator, '\n');
            }
            try input_buf.appendSlice(self.allocator, line);

            // Check if parens are balanced
            const balance = countParenBalance(input_buf.items);
            if (balance < 0) {
                // Too many closing parens - error
                try writer.writeAll("\x1b[1;31merror\x1b[0m: unexpected ')'\n");
                try writer.flush();
                input_buf.clearRetainingCapacity();
                continue;
            }
            if (balance > 0) {
                // Incomplete - continue reading
                continue;
            }

            // Parens balanced - evaluate
            const trimmed_input = std.mem.trim(u8, input_buf.items, " \t\r\n");
            if (trimmed_input.len > 0) {
                self.evalPrint(trimmed_input, writer) catch {};
                try writer.flush();
            }
            input_buf.clearRetainingCapacity();
        }
    }

    /// Count paren balance: positive = open parens, negative = too many close parens
    fn countParenBalance(input: []const u8) i32 {
        var balance: i32 = 0;
        var in_string = false;
        var in_comment = false;
        var i: usize = 0;

        while (i < input.len) : (i += 1) {
            const c = input[i];

            if (in_comment) {
                if (c == '\n') in_comment = false;
                continue;
            }

            if (in_string) {
                if (c == '\\' and i + 1 < input.len) {
                    i += 1; // Skip escaped char
                } else if (c == '"') {
                    in_string = false;
                }
                continue;
            }

            switch (c) {
                '"' => in_string = true,
                ';' => in_comment = true,
                '(' => balance += 1,
                ')' => {
                    balance -= 1;
                    if (balance < 0) return balance;
                },
                else => {},
            }
        }

        // If in string, consider incomplete
        if (in_string) return 1;

        return balance;
    }

    /// Run the REPL loop (for testing with anytype readers)
    pub fn run(self: *Repl, in_reader: anytype, writer: anytype) !void {
        _ = self;
        _ = in_reader;
        _ = writer;
        // This version is for tests only - use runWithFiles for actual REPL
    }

    /// Error information for better diagnostics
    pub const ErrorInfo = struct {
        kind: ErrorKind,
        line: u32,
        column: u32,
        text: []const u8,
    };

    pub const ErrorKind = enum {
        parse_unexpected_token,
        parse_unterminated_list,
        parse_invalid_number,
        compile_unbound_variable,
        compile_invalid_syntax,
        runtime_type_mismatch,
        runtime_user_error,
        other,
    };

    /// Evaluate a string and print the result, with nice error messages
    pub fn evalPrint(self: *Repl, source: []const u8, writer: anytype) !void {
        var err_info: ?ErrorInfo = null;
        const result = self.evalCapturingError(source, &err_info) catch |err| {
            if (err_info) |info| {
                try self.printDiagnostic(source, info, writer);
            } else {
                try writer.print("Error: {s}\n", .{@errorName(err)});
            }
            return err;
        };
        try self.printValue(result, writer);
        try writer.writeAll("\n");
    }

    fn printDiagnostic(self: *Repl, source: []const u8, info: ErrorInfo, writer: anytype) !void {
        _ = self;
        // Format: error: message at line:column
        const msg = switch (info.kind) {
            .parse_unexpected_token => "unexpected token",
            .parse_unterminated_list => "unterminated list",
            .parse_invalid_number => "invalid number",
            .compile_unbound_variable => "unbound variable",
            .compile_invalid_syntax => "invalid syntax",
            .runtime_type_mismatch => "type mismatch",
            .runtime_user_error => "user error",
            .other => "error",
        };

        try writer.print("\x1b[1;31merror\x1b[0m: {s}\n", .{msg});
        try writer.print("  \x1b[1;34m-->\x1b[0m <repl>:{d}:{d}\n", .{ info.line, info.column });
        try writer.print("   \x1b[1;34m|\x1b[0m\n", .{});

        // Print the source line
        var line_num: u32 = 1;
        var line_start: usize = 0;
        for (source, 0..) |c, i| {
            if (line_num == info.line) {
                // Find end of line
                var line_end = i;
                while (line_end < source.len and source[line_end] != '\n') line_end += 1;
                try writer.print("\x1b[1;34m{d:>3} |\x1b[0m {s}\n", .{ line_num, source[line_start..line_end] });
                break;
            }
            if (c == '\n') {
                line_num += 1;
                line_start = i + 1;
            }
        } else {
            // Single line input
            try writer.print("\x1b[1;34m  1 |\x1b[0m {s}\n", .{source});
        }

        // Print caret pointing to error
        try writer.print("   \x1b[1;34m|\x1b[0m ", .{});
        var col: u32 = 1;
        while (col < info.column) : (col += 1) {
            try writer.writeAll(" ");
        }
        try writer.print("\x1b[1;31m^\x1b[0m", .{});
        if (info.text.len > 1) {
            for (info.text[1..]) |_| {
                try writer.print("\x1b[1;31m^\x1b[0m", .{});
            }
        }
        try writer.print(" {s}\n", .{info.text});
    }

    /// Evaluate a string, capture error info for diagnostics
    fn evalCapturingError(self: *Repl, source: []const u8, err_info: *?ErrorInfo) !Value {
        // Use arena for IR nodes to simplify cleanup
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Parse
        var parser = Parser.init(arena_alloc, self.heap, source);
        defer parser.deinit();

        var expr = parser.parse() catch |err| {
            const loc = parser.getErrorLocation();
            err_info.* = .{
                .kind = switch (err) {
                    error.UnexpectedToken => .parse_unexpected_token,
                    error.UnterminatedList => .parse_unterminated_list,
                    error.InvalidNumber => .parse_invalid_number,
                    else => .other,
                },
                .line = loc.line,
                .column = loc.column,
                .text = loc.text,
            };
            return error.ParseError;
        };

        // Check for defmacro
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Expand macros before compilation
        expr = self.expandMacros(expr) catch return error.CompileError;

        // Compile - use persistent compiler for globals, but temp builder/allocator
        // Save and restore since they use arena allocator
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(expr, &env) catch |err| {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            err_info.* = .{
                .kind = if (err == error.UnboundVariable) .compile_unbound_variable else .compile_invalid_syntax,
                .line = 1,
                .column = 1,
                .text = "",
            };
            return error.CompileError;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Emit bytecode (with heap for symbol interning)
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);

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

        // Store child chunks persistently (closures need them beyond this eval)
        // Store the base index for this eval's chunks
        const chunk_base: u16 = @intCast(self.persistent_chunk_ptrs.items.len);
        for (child_chunks) |child| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(chunk.code);
                self.allocator.free(chunk.constants);
                return error.EmitError;
            };
            chunk_ptr.* = child;
            // Patch make_closure indices to absolute
            patchMakeClosureIndices(chunk_ptr.code, chunk_base);

            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(chunk.code);
                self.allocator.free(chunk.constants);
                return error.EmitError;
            };
        }

        // Free child chunk array (but not the contents, now owned by persistent storage)
        self.allocator.free(child_chunks);
        emitter.deinit();

        // Patch main chunk as well
        patchMakeClosureIndices(chunk.code, chunk_base);

        // Set chunk pool - VM uses absolute indices now
        self.vm.setChunkPool(self.persistent_chunk_ptrs.items);
        const result = self.vm.run(&chunk) catch |err| {
            self.allocator.free(chunk.code);
            self.allocator.free(chunk.constants);
            err_info.* = .{
                .kind = if (err == error.UserError) .runtime_user_error else .runtime_type_mismatch,
                .line = 1,
                .column = 1,
                .text = "",
            };
            return error.RuntimeError;
        };
        self.allocator.free(chunk.code);
        self.allocator.free(chunk.constants);
        return result;
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

        var expr = parser.parse() catch return error.ParseError;

        // Check for defmacro
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Expand macros before compilation
        expr = self.expandMacros(expr) catch return error.CompileError;

        // Compile - use persistent compiler for globals, but temp builder/allocator
        // Save and restore since they use arena allocator
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(expr, &env) catch |err| {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            return if (err == error.UnboundVariable) error.CompileError else error.CompileError;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Emit bytecode (with heap for symbol interning)
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);

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
        const chunk_base: u16 = @intCast(self.persistent_chunk_ptrs.items.len);
        for (child_chunks) |c| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
            chunk_ptr.* = c;
            // Patch make_closure indices to absolute
            patchMakeClosureIndices(chunk_ptr.code, chunk_base);
            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.free(c.code);
                self.allocator.free(c.constants);
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
        }
        self.allocator.free(child_chunks);

        // Patch main chunk as well
        patchMakeClosureIndices(chunk.code, chunk_base);

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

        // Set chunk pool - VM uses absolute indices now
        self.vm.setChunkPool(self.persistent_chunk_ptrs.items);
        return self.vm.run(&chunk) catch return error.RuntimeError;
    }

    /// Print a value in Lisp notation
    pub fn printValue(self: *Repl, val: Value, writer: anytype) anyerror!void {
        if (val.isNil()) {
            try writer.writeAll("nil");
        } else if (val.isFixnum()) {
            try writer.print("{d}", .{val.toFixnum()});
        } else if (val.isFloat()) {
            try writer.print("{d}", .{val.toFloat()});
        } else if (val.isCharacter()) {
            const cp = val.toCharacter();
            // Named characters
            if (cp == ' ') {
                try writer.writeAll("#\\space");
            } else if (cp == '\n') {
                try writer.writeAll("#\\newline");
            } else if (cp == '\t') {
                try writer.writeAll("#\\tab");
            } else if (cp == '\r') {
                try writer.writeAll("#\\return");
            } else if (cp >= 32 and cp < 127) {
                try writer.print("#\\{c}", .{@as(u8, @intCast(cp))});
            } else {
                try writer.print("#\\U+{X:0>4}", .{cp});
            }
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
        } else if (val.isHashTable()) {
            const ht = val.toPtr(runtime.HashTable);
            try writer.print("#<hash-table count={d}>", .{ht.count});
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
        } else if (std.mem.startsWith(u8, cmd, ",l ") or std.mem.startsWith(u8, cmd, ",load ")) {
            const path = if (std.mem.startsWith(u8, cmd, ",l "))
                std.mem.trim(u8, cmd[3..], " \t")
            else
                std.mem.trim(u8, cmd[6..], " \t");
            self.loadFile(path, writer) catch |err| {
                try writer.print("Load error: {s}\n", .{@errorName(err)});
            };
        } else if (std.mem.eql(u8, cmd, ",h") or std.mem.eql(u8, cmd, ",help")) {
            try writer.writeAll(
                \\Commands:
                \\  ,q ,quit       Exit REPL
                \\  ,d ,disasm     Toggle disassembly display
                \\  ,l ,load FILE  Load and evaluate a file
                \\  ,h ,help       Show this help
                \\
            );
        } else {
            try writer.print("Unknown command: {s}\n", .{cmd});
        }
    }

    /// Load and evaluate a file (public for main.zig)
    pub fn loadFilePublic(self: *Repl, path: []const u8, writer: anytype) !void {
        return self.loadFile(path, writer);
    }

    /// Load and evaluate a file
    fn loadFile(self: *Repl, path: []const u8, writer: anytype) !void {
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            try writer.print("Cannot open '{s}': {s}\n", .{ path, @errorName(err) });
            return error.IoError;
        };
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch |err| {
            try writer.print("Cannot read '{s}': {s}\n", .{ path, @errorName(err) });
            return error.IoError;
        };
        defer self.allocator.free(content);

        // Evaluate all expressions in the file
        try self.evalFileContent(content, writer);
        try writer.print("; loaded {s}\n", .{path});
    }

    /// Evaluate file content (multiple expressions)
    pub fn evalFileContent(self: *Repl, content: []const u8, writer: anytype) !void {
        var pos: usize = 0;

        while (pos < content.len) {
            // Skip whitespace and comments
            while (pos < content.len) {
                if (content[pos] == ' ' or content[pos] == '\t' or
                    content[pos] == '\n' or content[pos] == '\r')
                {
                    pos += 1;
                } else if (content[pos] == ';') {
                    // Skip comment line
                    while (pos < content.len and content[pos] != '\n') {
                        pos += 1;
                    }
                } else {
                    break;
                }
            }

            if (pos >= content.len) break;

            // Find end of expression (simple approach: match parens)
            const start = pos;
            const end = self.findExprEnd(content, pos) catch |err| {
                try writer.print("Parse error at position {d}: {s}\n", .{ pos, @errorName(err) });
                return error.ParseError;
            };

            if (end > start) {
                const expr = content[start..end];
                _ = self.eval(expr) catch |err| {
                    try writer.print("Error evaluating: {s}\n  {s}\n", .{ expr[0..@min(50, expr.len)], @errorName(err) });
                    return err;
                };
                pos = end;
            } else {
                break;
            }
        }
    }

    /// Find end of S-expression
    fn findExprEnd(self: *Repl, content: []const u8, start: usize) !usize {
        _ = self;
        var pos = start;
        if (pos >= content.len) return start;

        // Handle list
        if (content[pos] == '(') {
            var depth: usize = 1;
            pos += 1;
            while (pos < content.len and depth > 0) {
                if (content[pos] == '(') {
                    depth += 1;
                } else if (content[pos] == ')') {
                    depth -= 1;
                } else if (content[pos] == '"') {
                    // Skip string
                    pos += 1;
                    while (pos < content.len and content[pos] != '"') {
                        if (content[pos] == '\\' and pos + 1 < content.len) {
                            pos += 1;
                        }
                        pos += 1;
                    }
                } else if (content[pos] == ';') {
                    // Skip comment
                    while (pos < content.len and content[pos] != '\n') {
                        pos += 1;
                    }
                    pos -= 1; // will be incremented below
                }
                pos += 1;
            }
            if (depth > 0) return error.ParseError;
            return pos;
        }

        // Handle atom
        while (pos < content.len) {
            const c = content[pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or
                c == '(' or c == ')' or c == ';')
            {
                break;
            }
            pos += 1;
        }
        return pos;
    }

    // ========================================================================
    // Macro support
    // ========================================================================

    /// Check if expression is (defmacro name (args) body)
    fn isDefmacro(self: *Repl, expr: Value) bool {
        if (!expr.isCons()) return false;
        const cons = expr.toPtr(Cons);
        if (!cons.car.isSymbol()) return false;

        if (self.compiler.builtins) |b| {
            return cons.car.raw == b.defmacro.raw;
        }

        const sym = cons.car.toPtr(Symbol);
        return std.mem.eql(u8, sym.getName(), "defmacro");
    }

    /// Handle defmacro: compile the macro body and store the closure
    /// (defmacro name (args...) body...) -> stores (lambda (args...) body...) as macro
    fn handleDefmacro(self: *Repl, expr: Value, arena_alloc: std.mem.Allocator) !Value {
        // Extract: (defmacro name (args...) body...)
        const cons1 = expr.toPtr(Cons);
        const rest1 = cons1.cdr;
        if (!rest1.isCons()) return error.CompileError;

        const cons2 = rest1.toPtr(Cons);
        if (!cons2.car.isSymbol()) return error.CompileError;
        const name_sym = cons2.car.toPtr(Symbol);
        const name = name_sym.getName();

        const rest2 = cons2.cdr;
        if (!rest2.isCons()) return error.CompileError;

        // Build (lambda (args...) body...) to evaluate
        const lambda_sym = self.heap.intern("lambda") orelse return error.CompileError;
        const lambda_expr = self.heap.allocCons(lambda_sym, rest2) orelse return error.CompileError;

        // Compile and evaluate the lambda to get a closure
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = self.compiler.compile(lambda_expr, &env) catch |err| {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            return if (err == error.UnboundVariable) error.CompileError else error.CompileError;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
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

        // Add child chunks
        const chunk_base: u16 = @intCast(self.persistent_chunk_ptrs.items.len);
        for (child_chunks) |c| {
            const chunk_ptr = self.allocator.create(bytecode.Chunk) catch {
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
            chunk_ptr.* = c;
            patchMakeClosureIndices(chunk_ptr.code, chunk_base);
            self.persistent_chunk_ptrs.append(self.allocator, chunk_ptr) catch {
                self.allocator.free(c.code);
                self.allocator.free(c.constants);
                self.allocator.destroy(chunk_ptr);
                self.allocator.free(child_chunks);
                return error.EmitError;
            };
        }
        self.allocator.free(child_chunks);

        var mutable_chunk = chunk;
        patchMakeClosureIndices(mutable_chunk.code, chunk_base);

        self.vm.setChunkPool(self.persistent_chunk_ptrs.items);
        const closure = self.vm.run(&mutable_chunk) catch return error.RuntimeError;

        if (!closure.isClosure()) return error.CompileError;

        // Store the closure in the macros table
        self.macros.put(name, closure) catch return error.CompileError;

        // Return the macro name as a symbol
        return cons2.car;
    }

    /// Expand macros in an expression (recursive)
    fn expandMacros(self: *Repl, expr: Value) ReplError!Value {
        // Non-list: no expansion
        if (!expr.isCons()) return expr;

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        // Check if head is a macro
        if (head.isSymbol()) {
            const sym = head.toPtr(Symbol);
            const name = sym.getName();

            // Skip special forms that shouldn't be expanded
            if (self.compiler.builtins) |b| {
                if (head.raw == b.quote.raw or head.raw == b.quasiquote.raw) {
                    return expr; // Don't expand inside quote
                }
            }

            if (self.macros.get(name)) |macro_closure| {
                // Expand macro: call the closure with the args
                const expansion = try self.callMacro(macro_closure, cons.cdr);
                // Recursively expand the result
                return self.expandMacros(expansion);
            }
        }

        // Recursively expand in subexpressions
        const expanded_car = try self.expandMacros(cons.car);
        const expanded_cdr = try self.expandMacroList(cons.cdr);

        // Rebuild cons if changed
        if (expanded_car.raw != cons.car.raw or expanded_cdr.raw != cons.cdr.raw) {
            return self.heap.allocCons(expanded_car, expanded_cdr) orelse return error.RuntimeError;
        }
        return expr;
    }

    /// Expand macros in a list (for cdr of cons)
    fn expandMacroList(self: *Repl, list: Value) ReplError!Value {
        if (!list.isCons()) return list;

        const cons = list.toPtr(Cons);
        const expanded_car = try self.expandMacros(cons.car);
        const expanded_cdr = try self.expandMacroList(cons.cdr);

        if (expanded_car.raw != cons.car.raw or expanded_cdr.raw != cons.cdr.raw) {
            return self.heap.allocCons(expanded_car, expanded_cdr) orelse return error.RuntimeError;
        }
        return list;
    }

    /// Call a macro closure with arguments (as a list)
    fn callMacro(self: *Repl, closure: Value, args: Value) ReplError!Value {
        // Build the function call: we need to apply the closure to the args
        // The args should NOT be evaluated - they're passed as-is (like quote)

        // Count args
        var argc: usize = 0;
        var arg_list = args;
        while (arg_list.isCons()) {
            argc += 1;
            arg_list = arg_list.toPtr(Cons).cdr;
        }

        // Push closure and args onto VM stack, then call
        // We'll generate bytecode to do this
        var code_buf: [256]u8 = undefined;
        var code_len: usize = 0;

        // push_const for closure (we'll add it as constant 0)
        code_buf[code_len] = @intFromEnum(Op.push_const);
        code_len += 1;
        std.mem.writeInt(u16, code_buf[code_len..][0..2], 0, .little);
        code_len += 2;

        // Push each arg as a constant (quoted values)
        var const_idx: u16 = 1;
        arg_list = args;
        while (arg_list.isCons()) {
            const arg_cons = arg_list.toPtr(Cons);
            _ = arg_cons; // We'll add the constant later
            code_buf[code_len] = @intFromEnum(Op.push_const);
            code_len += 1;
            std.mem.writeInt(u16, code_buf[code_len..][0..2], const_idx, .little);
            code_len += 2;
            const_idx += 1;
            arg_list = arg_list.toPtr(Cons).cdr;
        }

        // call instruction
        code_buf[code_len] = @intFromEnum(Op.call);
        code_len += 1;
        code_buf[code_len] = @intCast(argc);
        code_len += 1;

        // ret to return the result
        code_buf[code_len] = @intFromEnum(Op.ret);
        code_len += 1;

        // Build constants array
        var constants = self.allocator.alloc(u64, const_idx) catch return error.RuntimeError;
        defer self.allocator.free(constants);

        constants[0] = closure.raw;
        var idx: u16 = 1;
        arg_list = args;
        while (arg_list.isCons()) {
            const arg_cons = arg_list.toPtr(Cons);
            constants[idx] = arg_cons.car.raw;
            idx += 1;
            arg_list = arg_cons.cdr;
        }

        const chunk = bytecode.Chunk{
            .code = code_buf[0..code_len],
            .constants = constants,
            .arity = 0,
            .has_rest = false,
            .num_locals = 0,
            .name = "<macro-call>",
        };

        // Use a separate VM to avoid corrupting the main VM state
        var nested_vm = Vm.init(self.allocator, self.heap);
        nested_vm.setGlobalEnv(&self.compiler.globals);
        nested_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        nested_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        nested_vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));

        // Copy globals from main VM
        for (self.vm.globals, 0..) |g, i| {
            nested_vm.globals[i] = g;
        }
        nested_vm.num_globals = self.vm.num_globals;

        nested_vm.setChunkPool(self.persistent_chunk_ptrs.items);
        return nested_vm.run(&chunk) catch return error.RuntimeError;
    }
};

/// Patch make_closure instructions to use absolute chunk indices
fn patchMakeClosureIndices(code: []u8, base: u16) void {
    var i: usize = 0;
    while (i < code.len) {
        const op: Op = @enumFromInt(code[i]);
        const size = op.operandSize();

        if (op == .make_closure) {
            // make_closure has: u16 chunk_index, u8 num_captures
            // Patch the u16 index at code[i+1..i+3]
            const rel_idx = std.mem.readInt(u16, code[i + 1 ..][0..2], .little);
            const abs_idx = base + rel_idx;
            std.mem.writeInt(u16, code[i + 1 ..][0..2], abs_idx, .little);
        }

        i += 1 + size;
    }
}

/// Convenience function to evaluate a string
pub fn evalString(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var repl = Repl.init(allocator, heap, .{});
    defer repl.deinit();
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
