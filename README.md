# [中文](README.CN.md) | [English](README.md) | [Install & Configuration](https://zhuyadong.github.io/zoop-docs/en/guide/install)
## Zoop is a Zig OOP solution


## Define Class
```zig
pub const Human = struct {
    // zoop class must align == zoop.alignment
    name: []const u8 align(zoop.alignment),

    pub fn init(self: *Human, name: []const u8) void {
        self.name = name;
    }

    pub fn deinit(self: *Human) void {
        self.name = "";
    }

    pub fn getName(self: *const Human) []const u8 {
        return self.name;
    }
    pub fn setName(self: *Human, name: []const u8) void {
        self.name = name;
    }
};
```
## Inherit
```zig
pub const SubHuman = struct {
    super: Human align(zoop.alignment),

    pub fn init(self: *SubHuman, name: []const u8) void {
        self.super.init(name);
    }
};
```
## Define Interface
```zig
pub const IHuman = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getName(self: IHuman) []const u8 {
        return zoop.icall(self, "getName", .{});
    }
    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.icall(self, "setName", .{name});
    }
};
```
## Inherit + Implementation Interface + Override
```zig
pub const HumanWithIface = struct {
    pub const extends = .{ IHuman };
    // field name is not important, but must be the first one and align == zoop.alignment
    anyname: SubHuman align(zoop.alignment),

    pub fn init(self: *HumanWithIface, name: []const u8) void {
        self.anyname.init(name);
    }

    // override
    pub fn setName(self: *HumanWithIface, _:[]const u8) void {
        self.anyname.super.setName("Custom");
    }
};
```
## Test
```zig
test  {
    const t = std.testing;

    const Alien = struct {x:u8 align(zoop.alignment)};

    var hwi = try zoop.new(t.allocator, HumanWithIface);
    hwi.init("Name");
    defer zoop.destroy(hwi);

    try t.expect(zoop.as(hwi, Alien) == null);
    try t.expect(zoop.as(hwi, Human) != null);
    try t.expect(zoop.as(hwi, SubHuman) != null);
    try t.expect(zoop.as(hwi, IHuman) != null);
    try t.expect(zoop.as(hwi, zoop.IObject) != null);
    try t.expectEqualStrings(zoop.as(hwi, IHuman).?.getName(), "Name");
    try t.expectEqualStrings(zoop.cast(hwi, IHuman).getName(), "Name");
    try t.expectEqualStrings(zoop.cast(hwi, Human).getName(), "Name");

    // test override
    zoop.cast(hwi, IHuman).setName("NewName");
    try t.expectEqualStrings(zoop.cast(hwi, Human).getName(), "Custom");
    try t.expectEqualStrings(zoop.cast(hwi, SubHuman).super.getName(), "Custom");
    try t.expectEqualStrings(zoop.cast(hwi, IHuman).getName(), "Custom");
}
```