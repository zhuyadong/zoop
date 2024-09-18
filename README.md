# [中文](README.CN.md) | [English](README.md)
# Zoop is an OOP solution for Zig

## Install
In the project root directory:
```shell
zig fetch "git+https://github.com/zhuyadong/zoop.git" --save=zoop
```
If you want to install a specific version:
```shell
zig fetch "git+https://github.com/zhuyadong/zoop.git#<ref id>" --save=zoop
```

## Define the class
```zig
// Define a class Human
pub const Human = struct {
    // The first field of the zoop class must be aligned to `zoop.alignment`
    name: []const u8 align(zoop.alignment),
    age: u8 = 30,

    // If there is no cleanup work, you can not define `deinit`
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
## Creating and destroying class objects
```zig
const t = std.testing;

// Create a `Human` on the heap
var phuman = zoop.new(t.allocator, Human, null);
// If the class field has a default value, the object field will be initialized to the default value
try t.expect(phuman.age == 30);
// Destroy the object and release the memory. If the class defines `deinit`, it will be called first and then release the memory.
zoop.destroy(phuman);

// Create a `Human` on the stack
var human = zoop.make(Human, null);
// Access object fields through `ptr()`
try t.expect(human.ptr().age == 30);
// Clean up the object (call `deinit` if any). If there is no work to clean up, you don't need to call `zoop.destroy`
zoop.destroy(human.ptr());

// Both `zoop.new` and `zoop.make` support creation-time initialization
phuman = zoop.new(t.allocator, Human, .{.name = "HeapObj", .age = 1});
human = zoop.make(Human, .{.name = "StackObj", .age = 2});
try t.expect(phuman.age == 1);
try t.expect(human.ptr().age == 2);
```
## Inheritance
```zig
// Define `SuperMan`, inherit from `Human`, the parent class must be the first field and the alignment is `zoop.alignment`,
// The field name is arbitrary and does not have to be `super`, but it is recommended to use `super`
pub const SuperMan = struct {
    super: Human align(zoop.alignment),
    // SuperMan can live a long time, u8 can't satisfy it, we use u16
    age: u16 = 9999,

    pub fn getAge(self: *SuperMan) u16 {
        return self.age;
    }

    pub fn setAge(self: *SuperMan, age: u16) void {
        self.age = age;
    }
};

// First create a `SuperMan` object
var psuperman = zoop.new(t.allocator, SuperMan, null);
//Call parent class method
psuperman.super.setName("super");
// Or call the parent class method like this. This method is suitable for situations where the inheritance hierarchy is too deep and you don't know which parent class implements the `setName` method.
// In addition, since it is called `upcall`, it means that even if `SuperMan` implements `setName`,
// The following call will still call the `setName` method of the nearest parent class
zoop.upcall(psuperman, .setName, .{"super"});
// You can also flexibly access all fields in the class inheritance tree. For example, if you want to access the `Human.age` field, you can do this:
var phuman_age = zoop.getField(psuperman, "age", u8);
try t.expect(phuman_age.* == 30);
// Access `SuperMan.age`, you can do this:
var psuper_age = zoop.getField(psuperman, "age", u16);
try t.expect(psuper_age.* == 9999);
// Note that if two `age` are of the same type and both are called "age",
// The above `zoop.getField` call will cause a compilation error to avoid bugs
```
## Class type conversion
```zig
// First create a Human and a SuperMan
var phuman = zoop.new(t.allocator, Human, null);
var psuper = zoop.new(t.allocator, SuperMan, null);

// Subclasses can be converted to parent classes
t.expect(zoop.as(psuper, Human) != null);
t.expect(zoop.cast(psuper, Human).age == 30);
// The parent class cannot be converted to a subclass (if `zoop.cast` is used, a compilation error will occur)
t.expect(zoop.as(phuman, SuperMan) == null);

```
## Define the interface
```zig
// Define an interface `IName` for accessing names
pub const IName = struct {
    // The interface can only define two fields, `ptr` and `vptr`, and the names and types must be the same as below
    ptr: *anyopaque,
    vptr: *anyopaque,

    // Define the `getName` interface method
    pub fn getName(self: IHuman) []const u8 {
        return zoop.ical(self, .getName, .{});
    }
    // Define the `setName` interface method
    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.ical(self, .setName, .{name});
    }
    // Don't worry about what `zoop.icall` is, just follow it
};

// Define another interface `IAge` for accessing age
pub const IAge = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getAge(self: IHuman) u16 {
        return zoop.ical(self, .getAge, .{});
    }
    pub fn setAge(self: IHuman, age: u16) void {
        zoop.ical(self, .setAge, .{age});
    }
}
```
## Implementing the interface
```zig
// We let `Human` implement the `IName` interface
pub const Human = struct {
    pub const extends = .{IName};
    ...//Same as above code
};

// Let `SuperMan` implement the `IAge` interface
pub const SuperMan = struct {
    pub const extends = .{IAge};
    ...//Same as above code
}
// The interface implemented by the parent class is automatically implemented by the class, so `SuperMan` also implements `IName`, although it only declares that it implements `IAge`.
// A subclass can repeatedly declare that it implements an interface that has already been implemented by its parent class. This will not cause any problems and will not affect the results.
// For example, the following code is equivalent to the above:
pub const SuperMan = struct {
    pub const extends = .{IAge, IName};
    ...
}
```
## Converting between classes and interfaces
```zig
// First create a Human and a SuperMan
var phuman = zoop.new(t.allocator, Human, .{.name = "human"});
var psuper = zoop.new(t.allocator, SuperMan, .{.super = .{.name = "super"}});

// Human implements IName, so it can be converted
var iname = zoop.cast(phuman, IName);
// SuperMan implements IAge, so it can be transferred
var iage = zoop.cast(psuper, IAge);
try t.expect(iage.getAge() == psuper.age);
try t.expectEqualStrings(iname.getName(), phuman.name);
// Human does not implement IAge, so the conversion will fail. (Note that now `iname` points to `phuman`, and `iage` points to `psuper`)
try t.expect(zoop.as(phuman, IAge) == null);
try t.expect(zoop.as(iname, IAge) == null);
// Now let iname point to psuper
iname = zoop.cast(psuper, IName);
// Or you can write it like this, but the performance is a little affected (`cast` is O(1), while `as` is O(n) in the worst case n=the number of interfaces implemented by SuperMan)
iname = zoop.as(psuper, IName).?;
// Now iname can be converted to IAge
try t.expect(zoop.as(iname, IAge) != null);
try t.expectEqualStrings(iname.getName(), "super");
// Everything can be converted to zoop.IObject
try t.expect(zoop.as(phuman, zoop.IObject) != null);
try t.expect(zoop.as(psuper, zoop.IObject) != null);
// Can also be converted back from IObject
var iobj = zoop.cast(psuper, zoop.IObject);
try t.expect(zoop.as(iobj, SuperMan).? == psuper);
```
To summarize `cast` and `as`:
- `cast` is applicable
    - Subclass -> Parent class
    - Sub-interface -> Parent interface
    - Class -> Interfaces implemented by the class and its parent class
- `as` is applicable
    - All the cases where `cast` is applicable and not applicable (everything can be `as`)
## Method overriding and virtual method calls
```zig
// If SuperMan overrides the getName method
pub const SuperMan = struct {
    ...//Same as above

    pub fn getName(_: *SuperMan) []const u8 {
        return "override";
    }
}

// Now IName.getName will call SuperMan.getName instead of Human.getName
var psuper = zoop.new(t.allocator, SuperMan, .{.super = .{.name = "human"}});
var iname = zoop.cast(psuper, IName);
try t.expectEqualStrings(iname.getName(), "override");
// Another style of calling interface methods
try t.expectEqualStrings(zoop.vcall(psuper, IName.getName, .{}), "override");
// Virtual method calls are also useful for converted classes
var phuman = zoop.cast(psuper, Human);
iname = zoop.cast(phuman, IName);
try t.expectEqualStrings(iname.getName(), "override");
try t.expectEqualStrings(zoop.vcall(phuman, IName.getName, .{}), "override");
```
Performance notes for `vcall`:
`vcall` will use `cast` when possible, and `as` otherwise
