/+
error on line 11: "foo" is already defined in a base class
error on line 15: "foo" is already defined in a base class
+/

class A {
  int foo
}

class B is A {
  int foo
}

class C is A {
  int foo
  this() { foo = 0 }
}
