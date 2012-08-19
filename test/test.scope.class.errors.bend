/+
error on line 8: "a" is already defined in this scope
error on line 13: "b" is already defined in this scope
+/

class A {
  int a
  static int a
}

class B {
  static int b
  int b
}
