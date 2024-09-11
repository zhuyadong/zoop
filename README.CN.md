# [中文](README.CN.md) | [English](README.md) | [安装配置](https://zhuyadong.github.io/zoop-docs/guide/install)
## Zoop 是一种 Zig 的 OOP 方案


## 定义类
```zig
pub const Human = struct {
    name: []const u8,

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
## 继承
```zig
pub const SubHuman = struct {
    super: Human,

    pub fn init(self: *SubHuman, name: []const u8) void {
        self.super.init(name);
    }
};
```
## 定义接口
```zig
pub const IHuman = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getName(self: IHuman) []const u8 {
        return zoop.vptr(self).getName(self.ptr);
    }
    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.vptr(self).setName(self.ptr, name);
    }
};
```
## 继承+实现接口+重写函数
```zig
pub const HumanWithIface = struct {
    pub const extends = .{ IHuman };
    // field name is not important, but must be the first one
    anyname: SubHuman

    pub fn init(self: *HumanWithIface, name: []const u8) void {
        self.anyname.init(name);
    }

    // 重写函数
    pub fn setName(self: *HumanWithIface, _:[]const u8) void {
        self.anyname.super.setName("Custom");
    }
};
```
## 测试
```zig
test  {
    const t = std.testing;

    const Alien = struct {};

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

    // 测试重写函数
    zoop.cast(hwi, IHuman).setName("NewName");
    try t.expectEqualStrings(zoop.cast(hwi, Human).getName(), "Custom");
    try t.expectEqualStrings(zoop.cast(hwi, SubHuman).super.getName(), "Custom");
    try t.expectEqualStrings(zoop.cast(hwi, IHuman).getName(), "Custom");
}
```