const std = @import("std");

const runtime = @import("../runtime/runtime.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const interp = @import("../interp/interp.zig");
const io = @import("../runtime/primitives/io.zig");

const compile_chunk = @import("compile_chunk.zig");

pub const Heap = runtime.Heap;
pub const Value = runtime.Value;
pub const Chunk = runtime.Chunk;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;

pub const Runner = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: Vm,
    comp: Compiler,
    chunk_pool: std.ArrayList(*Chunk),

    pub fn init(self: *Runner, allocator: std.mem.Allocator, heap: *Heap) !void {
        self.* = .{
            .allocator = allocator,
            .heap = heap,
            .vm = undefined,
            .comp = undefined,
            .chunk_pool = std.ArrayList(*Chunk){},
        };

        self.vm = try Vm.init(allocator, heap);
        errdefer self.vm.deinit();

        self.comp = try Compiler.initWithHeap(allocator, &self.vm);
        errdefer self.comp.deinit();

        self.vm.setGlobalEnv(&self.comp.globals);

        errdefer self.chunk_pool.deinit(allocator);
        self.vm.setChunkPool(self.chunk_pool.items);
    }

    pub fn deinit(self: *Runner) void {
        self.chunk_pool.deinit(self.allocator);
        self.comp.deinit();
        self.vm.deinit();
    }

    pub fn compile(self: *Runner, source: []const u8) !*Chunk {
        return try compile_chunk.compileChunk(self.allocator, self.heap, &self.vm, &self.comp, &self.chunk_pool, source);
    }

    pub fn run(self: *Runner, chunk: *const Chunk) !Value {
        return try self.vm.run(chunk);
    }

};

pub fn eval(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var r: Runner = undefined;
    try r.init(allocator, heap);
    defer r.deinit();

    const chunk = try r.compile(source);
    return try r.run(chunk);
}

pub fn valueToString(allocator: std.mem.Allocator, val: Value) ![]u8 {
    var buf = std.ArrayList(u8){};
    errdefer buf.deinit(allocator);

    var w = buf.writer(allocator);
    try io.writeValueToBuffer(val, w.any());
    return try buf.toOwnedSlice(allocator);
}
