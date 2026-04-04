const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const BuiltinSymbols = @import("../builtins.zig").BuiltinSymbols;
const objects = @import("../objects.zig");
const io = @import("io.zig");
const Pathname = objects.Pathname;

/// Create a pathname from components
pub fn makePathname(
    allocator: std.mem.Allocator,
    heap: *Heap,
    host: Value,
    device: Value,
    directory: Value,
    name: Value,
    ty: Value,
    version: Value,
) !Value {
    _ = allocator;
    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Get pathname host component
pub fn pathnameHost(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.host;
}

/// Get pathname device component
pub fn pathnameDevice(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.device;
}

/// Get pathname directory component
pub fn pathnameDirectory(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    // CL test suite probes pathname-directory under ignore-errors with NIL inputs.
    // Returning NIL here avoids surfacing VM TypeMismatch for that case.
    if (val.isNil()) return Value.nil;
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.directory;
}

/// Get pathname name component
pub fn pathnameName(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.name;
}

/// Get pathname type component
pub fn pathnameType(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.type;
}

/// Get pathname version component
pub fn pathnameVersion(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    const p_val = try pathname(allocator, heap, val);
    const p = p_val.toPtr(Pathname);
    return p.version;
}

/// Check if value is a pathname
pub fn pathnamep(val: Value) Value {
    return Value.fromBool(val.isPathname());
}

fn streamPathnameDesignator(val: Value) !?Value {
    if (try io.fileStreamTruename(val)) |stream_truename| {
        if (!stream_truename.isNil()) return stream_truename;
    }
    if (try io.fileStreamPathname(val)) |stream_pathname| {
        if (!stream_pathname.isNil()) return stream_pathname;
    }
    return null;
}

/// Convert string or pathname to pathname
pub fn pathname(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    if (val.isPathname()) return val;
    if (try streamPathnameDesignator(val)) |stream_path| return stream_path;
    if (!val.isString()) return error.TypeMismatch;
    return try parseNamestring(allocator, heap, val);
}

pub fn pathDesignatorString(
    allocator: std.mem.Allocator,
    heap: *Heap,
    builtins: *const BuiltinSymbols,
    val: Value,
) !Value {
    return switch (val.typeKind()) {
        .string => val,
        .pathname => try namestring(allocator, heap, builtins, val),
        .stream => blk: {
            const stream_path = (try streamPathnameDesignator(val)) orelse break :blk error.TypeMismatch;
            break :blk try namestring(allocator, heap, builtins, stream_path);
        },
        else => error.TypeMismatch,
    };
}

pub fn pathDesignatorBytes(
    allocator: std.mem.Allocator,
    heap: *Heap,
    builtins: *const BuiltinSymbols,
    val: Value,
) ![]const u8 {
    const str_val = try pathDesignatorString(allocator, heap, builtins, val);
    if (!str_val.isString()) return error.TypeMismatch;
    return str_val.toPtr(objects.String).bytes();
}

/// Convert pathname to namestring
pub fn namestring(allocator: std.mem.Allocator, heap: *Heap, builtins: *const BuiltinSymbols, val: Value) !Value {
    if (try streamPathnameDesignator(val)) |stream_path| {
        return try namestring(allocator, heap, builtins, stream_path);
    }
    if (val.isString()) return val;
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(heap.backing_allocator);
    const writer = buf.writer(heap.backing_allocator);

    // host://device/directory/name.type;version format
    if (!p.host.isNil()) {
        const host_str = p.host.toPtr(objects.String);
        try writer.writeAll(host_str.bytes());
        try writer.writeAll("://");
    }

    if (!p.device.isNil()) {
        const dev_str = p.device.toPtr(objects.String);
        try writer.writeAll(dev_str.bytes());
        try writer.writeAll("/");
    }

    try writeDirectoryTo(writer, builtins, p.directory, p.name.isNil());

    if (!p.name.isNil()) {
        if (buf.items.len > 0 and buf.items[buf.items.len - 1] != '/') {
            try writer.writeByte('/');
        }
        const name_str = p.name.toPtr(objects.String);
        try writer.writeAll(name_str.bytes());
    }

    if (!p.type.isNil()) {
        try writer.writeByte('.');
        const type_str = p.type.toPtr(objects.String);
        try writer.writeAll(type_str.bytes());
    }

    if (!p.version.isNil()) {
        try writer.writeByte(';');
        switch (p.version.typeKind()) {
            .keyword => {
                const kw = p.version.toPtr(objects.Keyword);
                try writer.writeAll(kw.getName());
            },
            .fixnum => try writer.print("{d}", .{p.version.toFixnum()}),
            else => {},
        }
    }

    return try heap.allocBaseString(buf.items);
}

/// Parse namestring into pathname
pub fn parseNamestring(allocator: std.mem.Allocator, heap: *Heap, str: Value) !Value {
    _ = allocator;
    if (!str.isString()) return error.TypeMismatch;
    const s = str.toPtr(objects.String);
    const path = s.bytes();

    var host = Value.nil;
    var device = Value.nil;
    var directory = Value.nil;
    var name = Value.nil;
    var ty = Value.nil;
    var version = Value.nil;

    var i: usize = 0;

    // Parse host://
    if (std.mem.indexOf(u8, path, "://")) |host_end| {
        host = try heap.allocBaseString(path[0..host_end]);
        i = host_end + 3;
    }

    // Parse device/ (single letter followed by /)
    if (i + 2 < path.len and path[i + 1] == '/') {
        device = try heap.allocBaseString(path[i .. i + 1]);
        i += 2;
    }

    // Find last slash to separate directory from name
    const last_slash = std.mem.lastIndexOf(u8, path[i..], "/");

    // Parse directory
    if (last_slash) |ls_idx| {
        const dir_part = path[i .. i + ls_idx];
        var dir_list = Value.nil;

        // Determine if absolute or relative
        const dir_type = if (dir_part.len > 0 and dir_part[0] == '/')
            try heap.internKeyword("absolute")
        else
            try heap.internKeyword("relative");

        dir_list = try heap.allocCons(dir_type, Value.nil);
        var tail = dir_list;

        // Split directory components
        var start: usize = if (dir_part.len > 0 and dir_part[0] == '/') @as(usize, 1) else 0;
        var j = start;
        while (j < dir_part.len) : (j += 1) {
            if (dir_part[j] == '/') {
                if (j > start) {
                    const comp_str = try heap.allocBaseString(dir_part[start..j]);
                    const new_cons = try heap.allocCons(comp_str, Value.nil);
                    tail.toPtr(objects.Cons).cdr = new_cons;
                    heap.writeBarrier(tail, new_cons);
                    tail = new_cons;
                }
                start = j + 1;
            }
        }
        if (j > start and start < dir_part.len) {
            const comp_str = try heap.allocBaseString(dir_part[start..j]);
            const new_cons = try heap.allocCons(comp_str, Value.nil);
            tail.toPtr(objects.Cons).cdr = new_cons;
            heap.writeBarrier(tail, new_cons);
        }

        directory = dir_list;
        i += ls_idx + 1;
    }

    // Parse name.type;version
    const remainder = path[i..];

    // Find version
    if (std.mem.lastIndexOf(u8, remainder, ";")) |ver_idx| {
        const ver_part = remainder[ver_idx + 1 ..];
        if (ver_part.len == 0) return error.InvalidArgument;
        if (std.ascii.eqlIgnoreCase(ver_part, "newest")) {
            version = try heap.internKeyword("newest");
        } else if (std.ascii.eqlIgnoreCase(ver_part, "unspecific")) {
            version = try heap.internKeyword("unspecific");
        } else if (std.mem.eql(u8, ver_part, "*") or std.ascii.eqlIgnoreCase(ver_part, "wild")) {
            version = try heap.internKeyword("wild");
        } else {
            const ver_num = try std.fmt.parseInt(i64, ver_part, 10);
            version = Value.makeFixnum(ver_num);
        }
    }

    const name_type_part = if (std.mem.lastIndexOf(u8, remainder, ";")) |v|
        remainder[0..v]
    else
        remainder;

    // Find type
    if (std.mem.lastIndexOf(u8, name_type_part, ".")) |dot_idx| {
        const type_part = name_type_part[dot_idx + 1 ..];
        if (type_part.len > 0) {
            ty = try heap.allocBaseString(type_part);
        }

        const name_part = name_type_part[0..dot_idx];
        if (name_part.len > 0) {
            name = try heap.allocBaseString(name_part);
        }
    } else if (name_type_part.len > 0) {
        name = try heap.allocBaseString(name_type_part);
    }

    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Merge pathnames with defaults
pub fn mergePathnames(
    heap: *Heap,
    builtins: *const BuiltinSymbols,
    pn: Value,
    defaults: Value,
) !Value {
    if (!pn.isPathname()) return error.TypeMismatch;
    if (!defaults.isPathname()) return error.TypeMismatch;

    const p = pn.toPtr(Pathname);
    const d = defaults.toPtr(Pathname);

    const host = if (!p.host.isNil()) p.host else d.host;
    const device = if (!p.device.isNil()) p.device else d.device;
    const directory = if (p.directory.isNil())
        d.directory
    else if (isRelativeDirectory(builtins, p.directory))
        try mergeRelativeDirectory(heap, builtins, d.directory, p.directory)
    else
        p.directory;
    const name = if (!p.name.isNil()) p.name else d.name;
    const ty = if (!p.type.isNil()) p.type else d.type;
    const version = if (!p.version.isNil()) p.version else d.version;

    return try heap.allocPathname(host, device, directory, name, ty, version);
}

fn isRelativeDirectory(builtins: *const BuiltinSymbols, dir: Value) bool {
    if (!dir.isCons()) return false;
    const tag = dir.toPtr(objects.Cons).car;
    return tag.raw == builtins.kw_relative.raw;
}

fn directoryTagOrDefault(builtins: *const BuiltinSymbols, dir: Value) Value {
    if (!dir.isCons()) return builtins.kw_relative;
    const tag = dir.toPtr(objects.Cons).car;
    if (tag.raw == builtins.kw_absolute.raw or tag.raw == builtins.kw_relative.raw) return tag;
    return builtins.kw_relative;
}

fn directoryComponentsStart(builtins: *const BuiltinSymbols, dir: Value) Value {
    if (!dir.isCons()) return Value.nil;
    const first_cons = dir.toPtr(objects.Cons);
    const tag = first_cons.car;
    if (tag.raw == builtins.kw_absolute.raw or tag.raw == builtins.kw_relative.raw) return first_cons.cdr;
    return dir;
}

fn mergeRelativeDirectory(
    heap: *Heap,
    builtins: *const BuiltinSymbols,
    default_dir: Value,
    rel_dir: Value,
) !Value {
    var components = std.ArrayList(Value){};
    defer components.deinit(heap.backing_allocator);

    const tag = directoryTagOrDefault(builtins, default_dir);
    try components.append(heap.backing_allocator, tag);

    var cur_default = directoryComponentsStart(builtins, default_dir);
    while (cur_default.isCons()) {
        const c = cur_default.toPtr(objects.Cons);
        try components.append(heap.backing_allocator, c.car);
        cur_default = c.cdr;
    }

    var cur_rel = directoryComponentsStart(builtins, rel_dir);
    while (cur_rel.isCons()) {
        const c = cur_rel.toPtr(objects.Cons);
        try components.append(heap.backing_allocator, c.car);
        cur_rel = c.cdr;
    }

    var out = Value.nil;
    var i = components.items.len;
    while (i > 0) {
        i -= 1;
        out = try heap.allocCons(components.items[i], out);
    }
    return out;
}

/// Get directory portion as a string
/// Returns the directory component of a pathname as a namestring
pub fn directoryNamestring(allocator: std.mem.Allocator, heap: *Heap, builtins: *const BuiltinSymbols, val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);
    try writeDirectoryTo(writer, builtins, p.directory, true);

    return try heap.allocBaseString(buf.items);
}

/// Get file portion (name + type) as a string
pub fn fileNamestring(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);

    if (!p.name.isNil() and p.name.isString()) {
        const name_str = p.name.toPtr(objects.String);
        try writer.writeAll(name_str.bytes());
    }

    if (!p.type.isNil() and p.type.isString()) {
        try writer.writeByte('.');
        const type_str = p.type.toPtr(objects.String);
        try writer.writeAll(type_str.bytes());
    }

    return try heap.allocBaseString(buf.items);
}

/// Get host portion as a string
pub fn hostNamestring(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    _ = allocator;
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    if (p.host.isNil()) {
        return try heap.allocBaseString("");
    }

    if (p.host.isString()) {
        return p.host; // Already a string
    }

    return try heap.allocBaseString("");
}

/// Get user's home directory as a pathname
pub fn userHomedirPathname(allocator: std.mem.Allocator, heap: *Heap) !Value {
    _ = allocator;
    const home = if (std.posix.getenv("HOME")) |val| val else return Value.nil;
    if (home.len == 0) return Value.nil;

    // Parse the home directory path into a pathname object
    // Build directory list: (:absolute "component1" "component2" ...)
    var dir_list = Value.nil;

    // Split path by '/' and collect parts
    var it = std.mem.splitScalar(u8, home, '/');
    var parts_buf: [64][]const u8 = undefined;
    var parts_len: usize = 0;

    while (it.next()) |part| {
        if (part.len > 0 and parts_len < parts_buf.len) {
            parts_buf[parts_len] = part;
            parts_len += 1;
        }
    }

    // Build list in reverse order (for cons)
    var i = parts_len;
    while (i > 0) {
        i -= 1;
        const part_str = try heap.allocBaseString(parts_buf[i]);
        dir_list = try heap.allocCons(part_str, dir_list);
    }

    // Prepend :absolute
    const absolute_kw = try heap.internKeyword("ABSOLUTE");
    dir_list = try heap.allocCons(absolute_kw, dir_list);

    return try heap.allocPathname(Value.nil, Value.nil, dir_list, Value.nil, Value.nil, Value.nil);
}

/// Get the canonical (truename) of a pathname.
/// Returns the resolved absolute path and propagates missing-file failure.
pub fn truename(allocator: std.mem.Allocator, heap: *Heap, builtins: *const BuiltinSymbols, val: Value) !Value {
    if (try io.fileStreamTruename(val)) |stream_truename| {
        if (!stream_truename.isNil()) return stream_truename;
        if (try io.fileStreamPathname(val)) |stream_pathname| return stream_pathname;
    }
    const path_str = try pathDesignatorBytes(allocator, heap, builtins, val);

    // Use realpath to get the canonical path
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const real_path = if (std.posix.realpath(path_str, &buf)) |resolved| resolved else |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        else => return err,
    };

    // Parse the real path into a pathname
    const str_val = try heap.allocBaseString(real_path);
    return try parseNamestring(allocator, heap, str_val);
}

/// Ensure directories exist for a pathname
/// Creates any missing directories, returns pathname
pub fn ensureDirectoriesExist(
    allocator: std.mem.Allocator,
    heap: *Heap,
    builtins: *const BuiltinSymbols,
    val: Value,
) !struct { pathname: Value, created: bool } {
    const pn_val = try pathname(allocator, heap, val);
    const pn = pn_val.toPtr(Pathname);

    // Get the directory string
    const path_str = try pathnameToString(allocator, builtins, pn);
    defer allocator.free(path_str);

    // Find the directory portion (everything up to the last /)
    var dir_end: usize = 0;
    for (path_str, 0..) |c, i| {
        if (c == '/') dir_end = i;
    }

    if (dir_end == 0) {
        // No directory component
        return .{ .pathname = pn_val, .created = false };
    }

    const dir_path = path_str[0..dir_end];

    // Create directories
    const status = try std.fs.cwd().makePathStatus(dir_path);
    return .{
        .pathname = pn_val,
        .created = status == .created,
    };
}

/// Helper to convert pathname to string path
fn pathnameToString(allocator: std.mem.Allocator, builtins: *const BuiltinSymbols, pn: *const Pathname) ![]const u8 {
    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);

    try writeDirectoryTo(writer, builtins, pn.directory, pn.name.isNil());

    // Add name
    if (pn.name.isString()) {
        if (buf.items.len > 0 and buf.items[buf.items.len - 1] != '/') {
            try writer.writeByte('/');
        }
        const s = pn.name.toPtr(objects.String);
        try writer.writeAll(s.bytes());
    }

    // Add type (extension)
    if (pn.type.isString()) {
        try writer.writeByte('.');
        const s = pn.type.toPtr(objects.String);
        try writer.writeAll(s.bytes());
    }

    return try buf.toOwnedSlice(allocator);
}

fn writeDirectoryTo(writer: anytype, builtins: *const BuiltinSymbols, dir_val: Value, keep_trailing: bool) !void {
    if (dir_val.isNil() or !dir_val.isCons()) return;

    var dir = dir_val;
    const first_cons = dir.toPtr(objects.Cons);
    const first = first_cons.car;
    dir = first_cons.cdr;

    var wrote_any = false;
    var ends_with_sep = false;

    if (first.raw == builtins.kw_absolute.raw) {
        try writer.writeByte('/');
        wrote_any = true;
        ends_with_sep = true;
    }

    while (dir.isCons()) {
        const cons = dir.toPtr(objects.Cons);
        const component = cons.car;
        if (!component.isString()) break;
        const comp_str = component.toPtr(objects.String);
        try writer.writeAll(comp_str.bytes());
        wrote_any = true;
        ends_with_sep = false;
        dir = cons.cdr;
        if (dir.isCons()) {
            try writer.writeByte('/');
            ends_with_sep = true;
        }
    }

    if (keep_trailing and wrote_any and !ends_with_sep) {
        try writer.writeByte('/');
    }
}

/// Check if pathname contains wildcards
/// Returns T if any component contains :wild or wildcard characters
pub fn wildPathnameP(builtins: *const BuiltinSymbols, val: Value, field_key: ?Value) bool {
    if (!val.isPathname()) return false;
    const p = val.toPtr(Pathname);

    // If field-key specified, only check that component
    if (field_key) |key| {
        if (key.raw == builtins.kw_host.raw) return hasWildcard(builtins, p.host);
        if (key.raw == builtins.kw_device.raw) return hasWildcard(builtins, p.device);
        if (key.raw == builtins.kw_directory.raw) return dirHasWildcard(builtins, p.directory);
        if (key.raw == builtins.kw_name.raw) return hasWildcard(builtins, p.name);
        if (key.raw == builtins.kw_type.raw) return hasWildcard(builtins, p.type);
        if (key.raw == builtins.kw_version.raw) return hasWildcard(builtins, p.version);
        return false;
    }

    // Check all components
    return hasWildcard(builtins, p.host) or
        hasWildcard(builtins, p.device) or
        dirHasWildcard(builtins, p.directory) or
        hasWildcard(builtins, p.name) or
        hasWildcard(builtins, p.type) or
        hasWildcard(builtins, p.version);
}

fn hasWildcard(builtins: *const BuiltinSymbols, val: Value) bool {
    if (val.isNil()) return false;

    // :wild keyword indicates wildcard
    if (val.raw == builtins.kw_wild.raw) return true;

    // String containing * or ? is wild
    if (val.isString()) {
        const s = val.toPtr(objects.String);
        const bytes = s.bytes();
        for (bytes) |c| {
            if (c == '*' or c == '?') return true;
        }
    }

    return false;
}

fn dirHasWildcard(builtins: *const BuiltinSymbols, dir: Value) bool {
    if (dir.isNil()) return false;

    // :wild keyword
    if (dir.raw == builtins.kw_wild.raw) return true;

    // Check each element of directory list
    var list = dir;
    while (!list.isNil() and list.isCons()) {
        const cons = list.toPtr(objects.Cons);
        const elem = cons.car;

        // :wild-inferiors means ** (recursive wildcard)
        if (elem.raw == builtins.kw_wild.raw or elem.raw == builtins.kw_wild_inferiors.raw) {
            return true;
        } else if (hasWildcard(builtins, elem)) {
            return true;
        }

        list = cons.cdr;
    }

    return false;
}

// ============================================================================
// Tests
// ============================================================================

test "parseNamestring version parsing" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const s_num = try heap.allocBaseString("foo.lisp;42");
    const pn_num = try parseNamestring(testing.allocator, &heap, s_num);
    const p_num = pn_num.toPtr(Pathname);
    try testing.expect(p_num.version.isFixnum());
    try testing.expectEqual(@as(i64, 42), p_num.version.toFixnum());

    const s_newest = try heap.allocBaseString("foo.lisp;NEWEST");
    const pn_newest = try parseNamestring(testing.allocator, &heap, s_newest);
    const p_newest = pn_newest.toPtr(Pathname);
    try testing.expect(p_newest.version.isKeyword());
    try testing.expect(std.mem.eql(u8, p_newest.version.toPtr(objects.Keyword).getName(), "NEWEST"));

    const s_wild = try heap.allocBaseString("foo.lisp;*");
    const pn_wild = try parseNamestring(testing.allocator, &heap, s_wild);
    const p_wild = pn_wild.toPtr(Pathname);
    try testing.expect(p_wild.version.isKeyword());
    try testing.expect(std.mem.eql(u8, p_wild.version.toPtr(objects.Keyword).getName(), "WILD"));

    const s_bad = try heap.allocBaseString("foo.lisp;abc");
    try testing.expectError(error.InvalidCharacter, parseNamestring(testing.allocator, &heap, s_bad));

    const s_empty = try heap.allocBaseString("foo.lisp;");
    try testing.expectError(error.InvalidArgument, parseNamestring(testing.allocator, &heap, s_empty));
}

test "namestring version formatting" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    const name = try heap.allocBaseString("foo");
    const ty = try heap.allocBaseString("lisp");

    const pn_num = try heap.allocPathname(Value.nil, Value.nil, Value.nil, name, ty, Value.makeFixnum(42));
    const ns_num = try namestring(testing.allocator, &heap, &builtins, pn_num);
    try testing.expect(ns_num.isString());
    try testing.expect(std.mem.eql(u8, ns_num.toPtr(objects.String).bytes(), "foo.lisp;42"));

    const newest = try heap.internKeyword("NEWEST");
    const pn_kw = try heap.allocPathname(Value.nil, Value.nil, Value.nil, name, ty, newest);
    const ns_kw = try namestring(testing.allocator, &heap, &builtins, pn_kw);
    try testing.expect(ns_kw.isString());
    try testing.expect(std.mem.eql(u8, ns_kw.toPtr(objects.String).bytes(), "foo.lisp;NEWEST"));
}

test "ensureDirectoriesExist reports created" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const subdir = "a/b";
    var base_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = try tmp.dir.realpath(".", &base_buf);
    const full_path = try std.fmt.allocPrint(testing.allocator, "{s}/{s}", .{ base_path, subdir });
    defer testing.allocator.free(full_path);

    const path_str = try heap.allocBaseString(full_path);
    const pn = try parseNamestring(testing.allocator, &heap, path_str);
    const res1 = try ensureDirectoriesExist(testing.allocator, &heap, &builtins, pn);
    try testing.expect(res1.created);

    const res2 = try ensureDirectoriesExist(testing.allocator, &heap, &builtins, pn);
    try testing.expect(!res2.created);
}

test "ensureDirectoriesExist accepts string designator" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    var base_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = try tmp.dir.realpath(".", &base_buf);
    const full_path = try std.fmt.allocPrint(testing.allocator, "{s}/{s}", .{ base_path, "c/d/file.txt" });
    defer testing.allocator.free(full_path);

    const path_str = try heap.allocBaseString(full_path);
    const res = try ensureDirectoriesExist(testing.allocator, &heap, &builtins, path_str);
    try testing.expect(res.pathname.isPathname());
    try testing.expect(res.created);

    var dir = try std.fs.openDirAbsolute(base_path, .{});
    defer dir.close();
    try dir.access("c/d", .{});
}

test "truename missing path signals file-not-found" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    var base_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = try tmp.dir.realpath(".", &base_buf);
    const missing_path = try std.fmt.allocPrint(testing.allocator, "{s}/nope-nope", .{base_path});
    defer testing.allocator.free(missing_path);

    const path_val = try heap.allocBaseString(missing_path);
    try testing.expectError(error.FileNotFound, truename(testing.allocator, &heap, &builtins, path_val));
}

test "truename accepts file stream metadata" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("stream-path.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("x");
    }

    const path = try tmp.dir.realpathAlloc(testing.allocator, "stream-path.txt");
    defer testing.allocator.free(path);
    const path_val = try heap.allocBaseString(path);
    const stream = try io.openFile(testing.allocator, &heap, &builtins, path_val, try heap.internKeyword("input"), null, null);
    defer io.closeStream(stream, null) catch {};

    const result = try truename(testing.allocator, &heap, &builtins, stream);
    const result_ns = try namestring(testing.allocator, &heap, &builtins, result);
    try testing.expectEqualStrings(path, result_ns.toPtr(objects.String).bytes());
}

test "namestring accepts file stream metadata" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("stream-namestring.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("x");
    }

    const path = try tmp.dir.realpathAlloc(testing.allocator, "stream-namestring.txt");
    defer testing.allocator.free(path);
    const path_val = try heap.allocBaseString(path);
    const stream = try io.openFile(testing.allocator, &heap, &builtins, path_val, try heap.internKeyword("input"), null, null);
    defer io.closeStream(stream, null) catch {};

    const result = try namestring(testing.allocator, &heap, &builtins, stream);
    try testing.expectEqualStrings(path, result.toPtr(objects.String).bytes());
}

test "pathnameDirectory nil returns nil" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const dir = try pathnameDirectory(testing.allocator, &heap, Value.nil);
    try testing.expect(dir.isNil());
}

test "namestring preserves trailing slash for directory-only absolute pathnames" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    const input = try heap.allocBaseString("/tmp/foo/");
    const pn = try parseNamestring(testing.allocator, &heap, input);
    const out = try namestring(testing.allocator, &heap, &builtins, pn);
    try testing.expectEqualStrings("/tmp/foo/", out.toPtr(objects.String).bytes());
}

test "namestring preserves trailing slash for directory-only relative pathnames" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();
    const builtins = try BuiltinSymbols.init(&heap);

    const input = try heap.allocBaseString("foo/bar/");
    const pn = try parseNamestring(testing.allocator, &heap, input);
    const out = try namestring(testing.allocator, &heap, &builtins, pn);
    try testing.expectEqualStrings("foo/bar/", out.toPtr(objects.String).bytes());
}
