# Zig Development Rules

## Version: Zig 0.15

**ALWAYS use Zig 0.15 APIs.** Common changes from 0.14:

## Conventions

### Allocator First
Allocator is ALWAYS the first argument to any function that allocates:
```zig
// RIGHT
pub fn init(allocator: std.mem.Allocator) Self { ... }
pub fn create(allocator: std.mem.Allocator, value: T) !*T { ... }

// WRONG
pub fn init(config: Config, allocator: std.mem.Allocator) Self { ... }
```

### ArrayList (now unmanaged)
```zig
// WRONG (0.14 style)
var list = std.ArrayList(T).init(allocator);
list.append(item);

// RIGHT (0.15 style)
var list = std.ArrayList(T){};
try list.append(allocator, item);
list.deinit(allocator);
```

### I/O
```zig
// WRONG (0.14 style)
const stdout = std.io.getStdOut();

// RIGHT (0.15 style)
const stdout = std.fs.File.stdout();
```

### Build API
```zig
// Use b.createModule() with root_module
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
- Use enum literals: `.name = .habu` not `.name = "habu"`
- Requires `.fingerprint` field

## Snapshot Testing: oh!snap

Use [oh!snap](https://github.com/mnemnion/ohsnap) for snapshot tests.

### Adding Dependency
```bash
zig fetch --save "https://github.com/mnemnion/ohsnap/archive/refs/tags/v0.4.1.tar.gz"
```

### Basic Usage
```zig
const OhSnap = @import("ohsnap");

test "snapshot test" {
    const oh = OhSnap{};
    const result = myFunction();
    try oh.snap(@src(),
        \\expected output here
    ).expectEqual(result);
}
```

Empty string to generate initial snapshot, add `<!update>` to update.

### build.zig Setup
```zig
if (b.lazyDependency("ohsnap", .{
    .target = target,
    .optimize = optimize,
})) |ohsnap_dep| {
    unit_tests.root_module.addImport("ohsnap", ohsnap_dep.module("ohsnap"));
}
```
