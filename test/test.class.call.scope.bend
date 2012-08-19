/+
error on line 13: no symbol "a"
error on line 13: cannot call <error>
error on line 20: no member "b" on type C
error on line 20: cannot call <error>
error on line 24: no symbol "b"
error on line 24: cannot call <error>
+/

class A {
  void a()
  static void testA() {
    a()
  }
}

class B {
  void b()
  void testB() {
    C.b()
  }
  class C {
    void testC() {
      b()
    }
  }
}
