/+
error on line 8: base type creates cycle
error on line 12: base type creates cycle
error on line 15: base type creates cycle
+/

class A is B {}
class B is A {}

class C is D {}
class D is E {}
class E is C {}

class F is F.G {
  class G is F {}
}
