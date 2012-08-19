/+
// class A
var A = function(a) {
  this.a = a;
};

// class B
var B = function(a, b) {
  A.call(this, a);
  this.b = b;
};
B.prototype = Object.create(A.prototype);
+/

class B is A { int b }
class A { int a }
