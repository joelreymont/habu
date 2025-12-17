# Zig 0.15 API Reference

Local reference for Zig 0.15 API changes and patterns.

## Comptime for Performance

Use `comptime` aggressively for zero-cost abstractions:

### Comptime Lookup Tables
```zig
// Generate character class lookup at compile time
const char_class = comptime blk: {
    var table: [256]CharType = .{.invalid} ** 256;
    for ('a'..='z') |c| table[c] = .alpha;
    for ('A'..='Z') |c| table[c] = .alpha;
    for ('0'..='9') |c| table[c] = .digit;
    break :blk table;
};

// Usage - single array lookup instead of branching
fn classifyChar(c: u8) CharType {
    return char_class[c];
}
```

### Force Inlining for Hot Paths
```zig
inline fn decodeFast(bytes: []const u8) u32 {
    // Compiler MUST inline - no function call overhead
}
```

### Comptime String Operations
```zig
// BEST - comptime when possible
const msg = comptime std.fmt.comptimePrint("error: {s}", .{"known"});
```

## Memory Allocation

### Alignment Enum

`alignedAlloc` uses `std.mem.Alignment` enum instead of raw integers:

```zig
// 16-byte aligned allocation
const mem = try allocator.alignedAlloc(u8, .@"16", size);

// Available values: .@"1", .@"2", .@"4", .@"8", .@"16", .@"32", .@"64"
// Use null for natural alignment of the type
const mem = try allocator.alignedAlloc(u8, null, size);
```

### ArrayList (Unmanaged)

**CRITICAL**: `std.ArrayList(T)` no longer stores allocator. Pass allocator to every method:

```zig
// OLD (pre-0.15) - WRONG:
var list = std.ArrayList(T).init(allocator);
try list.append(item);
list.deinit();

// NEW (0.15) - CORRECT:
var list = std.ArrayList(T){};
try list.append(allocator, item);
const slice = try list.toOwnedSlice(allocator);
list.deinit(allocator);
```

### ArrayList Method Signatures (0.15)

| Method | Old | New |
|--------|-----|-----|
| init | `.init(alloc)` | `{}` or `.{}` |
| append | `.append(item)` | `.append(alloc, item)` |
| appendSlice | `.appendSlice(items)` | `.appendSlice(alloc, items)` |
| pop | `.pop()` | `.pop()` (no change) |
| toOwnedSlice | `.toOwnedSlice()` | `.toOwnedSlice(alloc)` |
| deinit | `.deinit()` | `.deinit(alloc)` |
| items | `.items` | `.items` (no change) |

### StringHashMap - Still Managed

`std.StringHashMap(V)` still uses `.init(allocator)` - it's managed.

```zig
var map = std.StringHashMap([]const u8).init(allocator);
defer map.deinit();
try map.put(key, value);
```

### ArenaAllocator

```zig
var arena = std.heap.ArenaAllocator.init(backing_allocator);
defer arena.deinit();
const alloc = arena.allocator();
// allocator.free() is a no-op - arena frees all at once
```

## I/O

### stdout Pattern

```zig
const std = @import("std");
const fs = std.fs;

pub fn main() !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;  // std.Io.Writer

    try w.print("Hello {s}\n", .{"world"});
    try w.writeAll("raw bytes");
    try w.writeByte('x');
    try w.flush();
}
```

### File.Writer Structure

```zig
pub const Writer = struct {
    file: File,
    err: ?WriteError = null,
    interface: std.Io.Writer,  // <-- use this for print/writeAll
};
```

### std.Io.Writer Methods

| Method | Signature | Notes |
|--------|-----------|-------|
| `print` | `fn(*Writer, comptime []const u8, anytype) Error!void` | Formatted output |
| `writeAll` | `fn(*Writer, []const u8) Error!void` | Write all bytes |
| `writeByte` | `fn(*Writer, u8) Error!void` | Single byte |
| `write` | `fn(*Writer, []const u8) Error!usize` | Partial write OK |
| `flush` | `fn(*Writer) Error!void` | Drain buffer |

### No writeByteNTimes - Use Loop

```zig
fn writeByteNTimes(w: *std.Io.Writer, byte: u8, n: usize) !void {
    for (0..n) |_| try w.writeByte(byte);
}
```

### File Reading

```zig
const file = try fs.openFileAbsolute(path, .{});
defer file.close();
const content = try file.readToEndAlloc(allocator, max_size);
defer allocator.free(content);
```

### File.Reader (buffered line reading)

```zig
const file = try fs.openFileAbsolute(path, .{});
defer file.close();

var read_buf: [64 * 1024]u8 = undefined;
var file_reader = fs.File.Reader.init(file, &read_buf);
const reader = &file_reader.interface;

while (true) {
    const line = reader.takeDelimiter('\n') catch |err| switch (err) {
        error.StreamTooLong => {
            _ = reader.discardDelimiterExclusive('\n') catch break;
            continue;
        },
        error.ReadFailed => break,
    } orelse break;
    // process line...
}
```

## Build System

### build.zig

```zig
const exe = b.addExecutable(.{
    .name = "name",
    .root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    }),
});
```

### build.zig.zon

```zig
.{
    .name = .package_name,  // enum literal, not string
    .version = "0.1.0",
    .fingerprint = 0xABCD1234,  // required
    .dependencies = .{},
    .paths = .{ "build.zig", "build.zig.zon", "src" },
}
```

### Lazy Dependencies

```zig
if (b.lazyDependency("dep_name", .{
    .target = target,
    .optimize = optimize,
})) |dep| {
    exe.root_module.addImport("dep", dep.module("dep"));
}
```

## Formatting

### Custom Format Functions

Types with custom `format` method need explicit specifier:

```zig
// If type has format method, use {f} to call it
try writer.print("{f}", .{my_type});

// Use {any} for default debug formatting
try writer.print("{any}", .{my_type});

// Plain {} is ambiguous if type has format - causes compile error
```

## Testing

### Unused Variables

Zig 0.15 is stricter about unused mutable variables:

```zig
// ERROR: var never mutated
var x = something();
_ = x;

// FIX: use const
const x = something();
_ = x;
```

## Threading

### Per-thread Arena

```zig
// Per-thread arena to avoid allocator contention
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena.allocator();

// Atomic work stealing
var work_index = std.atomic.Value(usize).init(0);
const idx = work_index.fetchAdd(1, .monotonic);
```

## JSON

```zig
const parsed = try std.json.parseFromSlice(
    std.json.Value,
    allocator,
    json_string,
    .{}
);
defer parsed.deinit();
const obj = parsed.value.object;
```
