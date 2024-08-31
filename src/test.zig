const std = @import("std");
const zoop = @import("zoop.zig");
const testing = std.testing;
const assert = std.debug.assert;

const IObject = zoop.IObject;

// normal class
pub const Human = struct {
    // class must call zoop.Fn() on @This
    pub usingnamespace zoop.Fn(@This());

    // There must be a mixin(and must named 'mixin', but the position of declaration is not important),
    // and the type information is all here
    mixin: zoop.Mixin(@This()),
    name: []const u8,

    // Used by initializing heap allocated object.
    pub fn init(self: *Human, name: []const u8) void {
        self.name = name;
    }

    // Only the cleanup is done here.
    // deinit() is generally not called manually, please call desstroy() that zoop provided,
    // destroy() will call deinit() that inherits all classes on the chain,
    // and if the object is on the heap, it will also free up memory
    pub fn deinit(self: *Human) void {
        self.* = undefined;
    }

    // Fn() for methods that can be overrided and inherited
    pub fn Fn(comptime T: type) type {
        return zoop.Method(.{
            struct {
                pub fn getName(this: *T) []const u8 {
                    // T may be a subclass, so here we use cast to find Human's own memory location,
                    // and the cast operation is very cheap, just a pointer operation
                    const self: *Human = this.as(Human).?;
                    return self.name;
                }
            },
            struct {
                pub fn setName(this: *T, name: []const u8) void {
                    var self: *Human = this.as(Human).?;
                    self.name = name;
                }
            },
        });
    }
};

test "Human" {
    const t = std.testing;
    try t.expect(true);

    var stack: Human = Human.make();
    stack.initMixin();
    stack.init("stackhuman");
    defer stack.destroy();
    try t.expectEqualStrings(stack.getName(), "stackhuman");

    // the zoop.iobject interface is automatically implemented by human.
    try t.expect(stack.as(IObject) != null);

    // we can also come back through iobject.as()
    try t.expectEqualStrings(stack.as(IObject).?.as(Human).?.getName(), "stackhuman");

    var heap = try Human.new(t.allocator);
    heap.init("heaphuman");
    defer heap.destroy();
    try t.expectEqualStrings(heap.getName(), "heaphuman");
    heap.setName("human");
    try t.expectEqualStrings(heap.getName(), "human");
    try t.expectEqualStrings(heap.as(IObject).?.as(Human).?.getName(), "human");
    try t.expect(zoop.metaInfo(&stack) == stack.__meta__());
    try t.expect(zoop.metaInfo(stack.cast(IObject)) == stack.__meta__());
}

// sub class
pub const SubHuman = struct {
    // extends Human, extends is keyword.
    pub const extends = .{Human};
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()), // Human's data is in mixin

    pub fn init(self: *SubHuman, name: []const u8) void {
        self.as(Human).?.init(name);
    }

    // Note:There is no need to define a deinit() to call the parent class's deinit(),
    // all deinit() (if defined) on the inheritance chain will be automatically called by destroy().
};

test "SubHuman" {
    const t = std.testing;

    var sub = try SubHuman.new(t.allocator);
    sub.init("Human");
    defer sub.destroy(); // this will call Human.deinit() and free sub
    try t.expectEqualStrings(sub.getName(), "Human");
    sub.setName("HumanX");
    try t.expectEqualStrings(sub.getName(), "HumanX");
}

// define interface
pub const IHuman = struct {
    // interface must define Vtable, Vtable is keyword
    pub const Vtable = zoop.DefVtable(@This(), struct {
        getName: *const fn (*anyopaque) []const u8,
        setName: *const fn (*anyopaque, []const u8) void,
    });
    // interface must call zoop.Api() on @This
    pub usingnamespace zoop.Api(@This());

    // fatptr, ptr and vptr are keywords.
    ptr: *anyopaque,
    vptr: *Vtable,

    // must implements wrapper methods in function 'Api'
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

pub const HumanWithIface = struct {
    // extends class Human and interface IHuman.
    // because get/setName() already implemented in Human, so we don't need implement again.
    pub const extends = .{
        SubHuman,
        IHuman,
    };
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()),

    pub fn init(self: *HumanWithIface, name: []const u8) void {
        self.as(Human).?.init(name);
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

test "HumanWithIface" {
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

pub const IRobot = struct {
    // interface can only extends interfaces.
    pub const extends = .{IHuman};

    pub const Vtable = zoop.DefVtable(@This(), struct {
        setPower: *const fn (*anyopaque, on: bool) void,
        getPower: *const fn (*anyopaque) bool,
    });
    pub usingnamespace zoop.Api(@This());

    ptr: *anyopaque,
    vptr: *Vtable,

    // Note: IRobot now have 4 api: get/setName, get/setPower
    pub fn Api(comptime Iface: type) type {
        return struct {
            pub fn setPower(self: Iface, on: bool) void {
                self.vptr.setPower(self.ptr, on);
            }
            pub fn getPower(self: Iface) bool {
                return self.vptr.getPower(self.ptr);
            }
        };
    }
};

pub const Robot = struct {
    pub const extends = .{ Human, IRobot };
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()),
    power: bool,

    pub fn init(self: *Robot, name: []const u8, power: bool) void {
        self.power = power;
        self.as(Human).?.init(name);
    }

    // only need implements set/getPower(),
    // because get/setName() already implemented by super 'Human'
    pub fn Fn(comptime T: type) type {
        return zoop.Method(.{
            struct {
                pub fn getPower(this: *T) bool {
                    const self = this.as(Robot).?;
                    return self.power;
                }
            },
            struct {
                pub fn setPower(this: *T, power: bool) void {
                    var self = this.as(Robot).?;
                    self.power = power;
                }
            },
        });
    }
};

test "Robot" {
    const t = std.testing;

    var robot = try Robot.new(t.allocator);
    robot.init("Robot", true);
    defer robot.destroy();
    try t.expect(robot.getPower() == true);
    try t.expectEqualStrings(robot.getName(), "Robot");
    try t.expectEqualStrings(robot.as(IHuman).?.getName(), "Robot");
    try t.expectEqualStrings(robot.as(IRobot).?.getName(), "Robot");
    try t.expectEqualStrings(robot.as(IHuman).?.as(IRobot).?.getName(), "Robot");
    try t.expect(robot.as(IHuman).?.as(IRobot).?.getPower() == true);

    robot.as(IHuman).?.as(IRobot).?.setName("XRobot");
    robot.as(IHuman).?.as(Robot).?.setPower(false);
    try t.expectEqualStrings(robot.getName(), "XRobot");
    try t.expectEqualStrings(robot.as(IHuman).?.getName(), "XRobot");
    try t.expectEqualStrings(robot.as(IRobot).?.getName(), "XRobot");
    try t.expectEqualStrings(robot.as(IHuman).?.as(IRobot).?.getName(), "XRobot");
    try t.expect(robot.as(IObject).?.as(Robot).?.getPower() == false);
}

pub const NickRobot = struct {
    pub const extends = .{Robot};
    pub usingnamespace zoop.Fn(@This());

    mixin: zoop.Mixin(@This()),
    buf: [512]u8 = undefined,
    nick_name: []const u8,

    pub fn init(self: *NickRobot, nick: []const u8, name: []const u8) void {
        self.nick_name = nick;
        self.as(Robot).?.init(name, true);
    }

    pub fn Fn(comptime T: type) type {
        return zoop.Method(.{struct {
            pub fn getName(this: *T) []const u8 {
                const super_name = this.as(Robot).?.getName();
                var self = this.as(NickRobot).?;
                return std.fmt.bufPrint(&self.buf, "{s}.{s}", .{ self.nick_name, super_name }) catch @panic("OOM");
            }
        }});
    }
};

test "NickRobot" {
    const t = std.testing;
    try t.expect(true);

    var nick = NickRobot.make();
    nick.initMixin();
    nick.init("Nick", "Robot");
    defer nick.destroy();
    const human: *Human = nick.as(Human).?;
    try t.expect(@intFromPtr(human.mixin.meta.?.rootptr) == @intFromPtr(&nick));
    try t.expectEqualStrings(nick.getName(), "Nick.Robot");
    try t.expectEqualStrings(nick.as(IObject).?.as(IHuman).?.getName(), "Nick.Robot");
    try t.expectEqualStrings(nick.cast(Human).as(IObject).?.as(IHuman).?.getName(), "Nick.Robot");
    try t.expectEqualStrings(nick.cast(Human).getName(), "Robot");
    try t.expectEqualStrings(nick.cast(Robot).as(IHuman).?.getName(), "Nick.Robot");
    try t.expectEqualStrings(nick.as(IHuman).?.getName(), "Nick.Robot");
    try t.expectEqualStrings(nick.as(IHuman).?.as(NickRobot).?.getName(), "Nick.Robot");

    try t.expect(zoop.typeInfo(&nick) == zoop.metaInfo(&nick).typeinfo);
    try t.expect(zoop.typeInfo(&nick) == zoop.typeInfo(NickRobot));
    try t.expect(zoop.typeInfo(&nick) == zoop.typeInfo(nick.as(IObject).?));
    try t.expectEqualStrings(zoop.metaInfo(&nick).typename(), @typeName(NickRobot));

    var iobj: IObject = nick.cast(IObject);
    try t.expect(!iobj.isNil());
    iobj.setNil();
    try t.expect(iobj.isNil());
}

pub fn main() !void {}
