/+
// class A
var A = function() {};
A.prototype.a = function() {};
A.prototype.ab = function() {};
A.prototype.testA = function() {
  this.a();
  this.ab();
};
new A().a();
new A().ab();

// class B
var B = function() {
  A.call(this);
};
B.prototype = Object.create(A.prototype);
B.prototype.ab = function() {};
B.prototype.testB = function() {
  this.a();
  this.ab();
};
new B().a();
new B().ab();

// class C
var C = function() {};
C.prototype.c = function() {};
C.prototype.testC = function() {
  this.c();
};

// class D
C.D = function() {};
C.D.prototype.d = function() {};
C.D.prototype.testD = function() {
  this.d();
};
new C().c();
new C.D().d();
+/

class A {
  void a() {}
  void ab() {}
  void testA() {
    a()
    ab()
  }
}

A().a()
A().ab()

class B is A {
  over void ab() {}
  void testB() {
    a()
    ab()
  }
}

B().a()
B().ab()

class C {
  void c() {}
  void testC() {
    c()
  }

  class D {
    void d() {}
    void testD() {
      d()
    }
  }
}

C().c()
C.D().d()
