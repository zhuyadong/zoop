# [中文](README.CN.md) | [English](README.md) | [官网](https://zhuyadong.github.io/zoop-docs/)
## Zoop 是一种 Zig 的 OOP 方案

## 定义类
```zig
pub const Human = struct {
    pub usingnamespace zoop.Fn(@This());

    name: []const u8,
    mixin: zoop.Mixin(@This()),

    pub fn init(self: *Human, name: []const u8) void {
        self.name = name;
    }

    pub fn deinit(self: *Human) void {
        self.name = "";
    }

    pub fn Fn(comptime T: type) type {
        return zoop.Method(.{
            struct {
                pub fn getName(this: *T) []const u8 {
                    const self: *Human = this.cast(Human);
                    return self.name;
                }
            },
            struct {
                pub fn setName(this: *T, name: []const u8) void {
                    var self: *Human = this.cast(Human);
                    self.name = name;
                }
            },
        });
    }
};
```
## 继承
```zig
pub const SubHuman = struct {
    pub const extends = .{Human};
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()),

    pub fn init(self: *SubHuman, name: []const u8) void {
        self.cast(Human).init(name);
    }
};
```
## 定义接口
```zig
pub const IHuman = struct {
    pub const Vtable = zoop.DefVtable(@This(), struct {
        getName: *const fn (*anyopaque) []const u8,
        setName: *const fn (*anyopaque, []const u8) void,
    });
    pub usingnamespace zoop.Api(@This());

    ptr: *anyopaque,
    vptr: *Vtable,

    pub fn Api(comptime I: type) type {
        return struct {
            pub fn getName(self: I) []const u8 {
                return self.vptr.getName(self.ptr);
            }
            pub fn setName(self: I, name: []const u8) void {
                self.vptr.setName(self.ptr, name);
            }
        };
    }
};
```
## 继承+实现接口+重写函数
```zig
pub const HumanWithIface = struct {
    pub const extends = .{
        SubHuman,
        IHuman,
    };
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()),

    pub fn init(self: *HumanWithIface, name: []const u8) void {
        self.cast(SubHuman).init(name);
    }

    pub fn Fn(comptime T: type) type {
        return zoop.Method(.{
            struct {
                pub fn setName(this: *T, _: []const u8) void {
                    this.cast(Human).name = "CustomName";
                }
            },
        });
    }
};
```
## 测试
```zig
test  {
    const t = std.testing;

    const Alien = struct {
        pub usingnamespace zoop.Fn(@This());
        mixin: zoop.Mixin(@This()),
    };

    var hwi = try HumanWithIface.new(t.allocator);
    hwi.init("HumanWithIface");
    defer hwi.destroy();

    try t.expect(hwi.as(Alien) == null);
    try t.expect(hwi.as(Human) != null);
    try t.expect(hwi.as(SubHuman) != null);
    try t.expect(hwi.as(IHuman) != null);
    try t.expect(hwi.as(zoop.IObject) != null);
    try t.expect(hwi.as(IHuman).?.as(Human).?.as(SubHuman).?.as(HumanWithIface).?.as(zoop.IObject).?.as(Human).?.as(Alien) == null);

    try t.expect(hwi.cast(IHuman).eql(hwi.as(IHuman).?));
    try t.expect(hwi.cast(zoop.IObject).eql(hwi.as(zoop.IObject).?));
    try t.expect(hwi.cast(Human) == hwi.as(Human).?);
    try t.expect(hwi.cast(SubHuman) == hwi.as(SubHuman).?);

    try t.expect(hwi.asptr() == hwi.cast(zoop.IObject).asptr());
    try t.expect(hwi.asptr() == hwi.cast(IHuman).asptr());
    try t.expect(hwi.asptr() == hwi.cast(Human).asptr());
    try t.expect(hwi.asptr() == hwi.cast(SubHuman).asptr());

    try t.expectEqualStrings(hwi.getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.cast(Human).getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.cast(SubHuman).getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.cast(IHuman).getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.as(IHuman).?.getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.as(Human).?.getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.as(SubHuman).?.getName(), "HumanWithIface");
    try t.expectEqualStrings(hwi.as(IHuman).?.as(Human).?.as(SubHuman).?.as(HumanWithIface).?.as(Human).?.getName(), "HumanWithIface");

    hwi.cast(IHuman).setName("NewName");
    try t.expectEqualStrings(hwi.as(IHuman).?.getName(), "CustomName");
    try t.expectEqualStrings(hwi.as(Human).?.getName(), "CustomName");
    try t.expectEqualStrings(hwi.as(SubHuman).?.getName(), "CustomName");
    try t.expectEqualStrings(hwi.as(IHuman).?.as(Human).?.as(SubHuman).?.as(HumanWithIface).?.as(Human).?.getName(), "CustomName");

    hwi.cast(SubHuman).setName("NewName");
    try t.expectEqualStrings(hwi.as(IHuman).?.getName(), "NewName");
    try t.expectEqualStrings(hwi.as(Human).?.getName(), "NewName");
    try t.expectEqualStrings(hwi.as(SubHuman).?.getName(), "NewName");
    try t.expectEqualStrings(hwi.as(IHuman).?.as(Human).?.as(SubHuman).?.as(HumanWithIface).?.as(Human).?.getName(), "NewName");
}
```