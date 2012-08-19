/+
error on line 7: cannot instantiate abstract class "A"
error on line 10: cannot instantiate abstract class "B"
+/

class A { void method() }
A()

class B is A {}
B()

class C is A { over void method() {} }
C()
