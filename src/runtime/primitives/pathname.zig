const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const objects = @import("../objects.zig");
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
pub fn pathnameHost(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.host;
}

/// Get pathname device component
pub fn pathnameDevice(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.device;
}

/// Get pathname directory component
pub fn pathnameDirectory(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.directory;
}

/// Get pathname name component
pub fn pathnameName(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.name;
}

/// Get pathname type component
pub fn pathnameType(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.type;
}

/// Get pathname version component
pub fn pathnameVersion(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.version;
}

/// Check if value is a pathname
pub fn pathnamep(val: Value) Value {
    return Value.fromBool(val.isPathname());
}

/// Convert string or pathname to pathname
pub fn pathname(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    if (val.isPathname()) return val;
    if (!val.isString()) return error.TypeMismatch;
    return try parseNamestring(allocator, heap, val);
}

/// Convert pathname to namestring
pub fn namestring(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    _ = allocator;
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(heap.allocator);
    const writer = buf.writer(heap.allocator);

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

    // directory - list like (:absolute "foo" "bar") or (:relative "baz")
    if (!p.directory.isNil() and p.directory.isCons()) {
        var dir = p.directory;
        const first = dir.car();
        dir = dir.cdr();

        // :absolute starts with /, :relative has no prefix
        if (first.isKeyword()) {
            const kw = first.toPtr(objects.Keyword);
            if (std.ascii.eqlIgnoreCase(kw.getName(), "absolute")) {
                try writer.writeByte('/');
            }
        }

        while (!dir.isNil()) {
            const component = dir.car();
            if (component.isString()) {
                const comp_str = component.toPtr(objects.String);
                try writer.writeAll(comp_str.bytes());
                dir = dir.cdr();
                if (!dir.isNil()) try writer.writeByte('/');
            } else break;
        }
    }

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
        if (p.version.isKeyword()) {
            const kw = p.version.toPtr(objects.Keyword);
            try writer.writeAll(kw.getName());
        } else if (p.version.isFixnum()) {
            try writer.print("{d}", .{p.version.toFixnum()});
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
                    tail = new_cons;
                }
                start = j + 1;
            }
        }
        if (j > start and start < dir_part.len) {
            const comp_str = try heap.allocBaseString(dir_part[start..j]);
            const new_cons = try heap.allocCons(comp_str, Value.nil);
            tail.toPtr(objects.Cons).cdr = new_cons;
        }

        directory = dir_list;
        i += ls_idx + 1;
    }

    // Parse name.type;version
    const remainder = path[i..];

    // Find version
    if (std.mem.lastIndexOf(u8, remainder, ";")) |ver_idx| {
        const ver_part = remainder[ver_idx + 1 ..];
        if (std.mem.eql(u8, ver_part, "newest")) {
            version = try heap.internKeyword("newest");
        } else if (std.mem.eql(u8, ver_part, "unspecific")) {
            version = try heap.internKeyword("unspecific");
        } else {
            const ver_num = std.fmt.parseInt(i64, ver_part, 10) catch 0;
            version = Value.makeFixnum(ver_num);
        }
        // Continue parsing name.type before version
        i = remainder.len - (remainder.len - ver_idx);
    } else {
        i = remainder.len;
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
    pn: Value,
    defaults: Value,
) !Value {
    if (!pn.isPathname()) return error.TypeMismatch;
    if (!defaults.isPathname()) return error.TypeMismatch;

    const p = pn.toPtr(Pathname);
    const d = defaults.toPtr(Pathname);

    const host = if (!p.host.isNil()) p.host else d.host;
    const device = if (!p.device.isNil()) p.device else d.device;
    const directory = if (!p.directory.isNil()) p.directory else d.directory;
    const name = if (!p.name.isNil()) p.name else d.name;
    const ty = if (!p.type.isNil()) p.type else d.type;
    const version = if (!p.version.isNil()) p.version else d.version;

    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Get directory portion as a string
/// Returns the directory component of a pathname as a namestring
pub fn directoryNamestring(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);

    // directory - list like (:absolute "foo" "bar") or (:relative "baz")
    if (!p.directory.isNil() and p.directory.isCons()) {
        var dir = p.directory;
        const first = dir.toPtr(objects.Cons).car;
        dir = dir.toPtr(objects.Cons).cdr;

        if (first.isKeyword()) {
            const kw = first.toPtr(objects.Keyword);
            if (std.ascii.eqlIgnoreCase(kw.getName(), "absolute")) {
                try writer.writeByte('/');
            }
        }

        while (!dir.isNil() and dir.isCons()) {
            const cons = dir.toPtr(objects.Cons);
            const component = cons.car;
            if (component.isString()) {
                const comp_str = component.toPtr(objects.String);
                try writer.writeAll(comp_str.bytes());
                dir = cons.cdr;
                if (!dir.isNil()) try writer.writeByte('/');
            } else break;
        }

        // Add trailing slash for directory
        if (buf.items.len > 0 and buf.items[buf.items.len - 1] != '/') {
            try writer.writeByte('/');
        }
    }

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
    const home = std.posix.getenv("HOME") orelse return Value.nil;
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

/// Get the canonical (truename) of a pathname
/// Returns the resolved absolute path, or nil if file doesn't exist
pub fn truename(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    // Get the namestring from the pathname or string
    const needs_free = val.isPathname();
    const path_str = if (val.isPathname()) blk: {
        const pn = val.toPtr(Pathname);
        break :blk try pathnameToString(allocator, pn);
    } else if (val.isString()) blk: {
        const s = val.toPtr(objects.String);
        break :blk s.bytes();
    } else return error.TypeMismatch;
    defer if (needs_free) allocator.free(path_str);

    // Use realpath to get the canonical path
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const real_path = std.posix.realpath(path_str, &buf) catch |err| switch (err) {
        error.FileNotFound => return Value.nil,
        else => return error.FileError,
    };

    // Parse the real path into a pathname
    const str_val = try heap.allocBaseString(real_path);
    return try parseNamestring(allocator, heap, str_val);
}

/// Ensure directories exist for a pathname
/// Creates any missing directories, returns pathname
pub fn ensureDirectoriesExist(allocator: std.mem.Allocator, heap: *Heap, val: Value) !struct { pathname: Value, created: bool } {
    _ = heap;
    // Get the pathname
    const pn = if (val.isPathname())
        val.toPtr(Pathname)
    else if (val.isString())
        return error.TypeMismatch // Need to parse first
    else
        return error.TypeMismatch;

    // Get the directory string
    const path_str = try pathnameToString(allocator, pn);
    defer allocator.free(path_str);

    // Find the directory portion (everything up to the last /)
    var dir_end: usize = 0;
    for (path_str, 0..) |c, i| {
        if (c == '/') dir_end = i;
    }

    if (dir_end == 0) {
        // No directory component
        return .{ .pathname = val, .created = false };
    }

    const dir_path = path_str[0..dir_end];

    // Create directories
    std.fs.cwd().makePath(dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => return .{ .pathname = val, .created = false },
        else => return error.FileError,
    };

    return .{ .pathname = val, .created = true };
}

/// Helper to convert pathname to string path
fn pathnameToString(allocator: std.mem.Allocator, pn: *const Pathname) ![]const u8 {
    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);

    // Build path from directory
    if (pn.directory.isCons()) {
        var dir = pn.directory;
        var first_component = true;

        while (dir.isCons()) {
            const cons = dir.toPtr(objects.Cons);
            const part = cons.car;

            if (part.isKeyword()) {
                // :absolute or :relative
                const kw = part.toPtr(objects.Keyword);
                if (std.ascii.eqlIgnoreCase(kw.getName(), "absolute")) {
                    try writer.writeByte('/');
                }
                // :relative means start with nothing
            } else if (part.isString()) {
                if (!first_component) try writer.writeByte('/');
                const s = part.toPtr(objects.String);
                try writer.writeAll(s.bytes());
                first_component = false;
            }

            dir = cons.cdr;
        }
    }

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

/// Check if pathname contains wildcards
/// Returns T if any component contains :wild or wildcard characters
pub fn wildPathnameP(val: Value, field_key: ?Value) bool {
    if (!val.isPathname()) return false;
    const p = val.toPtr(Pathname);

    // If field-key specified, only check that component
    if (field_key) |key| {
        if (key.isKeyword()) {
            const kw = key.toPtr(objects.Keyword);
            const name = kw.getName();
            if (std.ascii.eqlIgnoreCase(name, "host")) return hasWildcard(p.host);
            if (std.ascii.eqlIgnoreCase(name, "device")) return hasWildcard(p.device);
            if (std.ascii.eqlIgnoreCase(name, "directory")) return dirHasWildcard(p.directory);
            if (std.ascii.eqlIgnoreCase(name, "name")) return hasWildcard(p.name);
            if (std.ascii.eqlIgnoreCase(name, "type")) return hasWildcard(p.type);
            if (std.ascii.eqlIgnoreCase(name, "version")) return hasWildcard(p.version);
        }
        return false;
    }

    // Check all components
    return hasWildcard(p.host) or
        hasWildcard(p.device) or
        dirHasWildcard(p.directory) or
        hasWildcard(p.name) or
        hasWildcard(p.type) or
        hasWildcard(p.version);
}

fn hasWildcard(val: Value) bool {
    if (val.isNil()) return false;

    // :wild keyword indicates wildcard
    if (val.isKeyword()) {
        const kw = val.toPtr(objects.Keyword);
        return std.ascii.eqlIgnoreCase(kw.getName(), "wild");
    }

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

fn dirHasWildcard(dir: Value) bool {
    if (dir.isNil()) return false;

    // :wild keyword
    if (dir.isKeyword()) {
        const kw = dir.toPtr(objects.Keyword);
        return std.ascii.eqlIgnoreCase(kw.getName(), "wild");
    }

    // Check each element of directory list
    var list = dir;
    while (!list.isNil() and list.isCons()) {
        const cons = list.toPtr(objects.Cons);
        const elem = cons.car;

        // :wild-inferiors means ** (recursive wildcard)
        if (elem.isKeyword()) {
            const kw = elem.toPtr(objects.Keyword);
            const name = kw.getName();
            if (std.ascii.eqlIgnoreCase(name, "wild") or
                std.ascii.eqlIgnoreCase(name, "wild-inferiors"))
            {
                return true;
            }
        } else if (hasWildcard(elem)) {
            return true;
        }

        list = cons.cdr;
    }

    return false;
}
