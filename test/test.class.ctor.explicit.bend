/+
// class A
var A = function(a) {
  this.a = a;
  this.b = 0;
};

// class B
var B = function(a, x) {
  this.y = 0;
  A.call(this, a);
  this.x = x;
};
B.prototype = Object.create(A.prototype);
new A(1);
new B(2, 3);
+/

class A {
  int a
  int b = 0
}
class B is A {
  int x
  int y = 0
  this(int a, int x) {
    base(a)
    this.x = x
  }
}
A(1)
B(2, 3)
