/+
// class A
var A = function() {};
A.a = function() {};
A.ab = function() {};
A.prototype.testA = function() {
  A.a();
  A.ab();
  A.a();
  A.ab();
};
A.a();
A.ab();

// class B
var B = function() {
  A.call(this);
};
B.prototype = Object.create(A.prototype);
B.ab = function() {};
B.prototype.testB = function() {
  A.a();
  B.ab();
  A.a();
  A.ab();
  B.a();
  B.ab();
};
B.a();
B.ab();

// class C
var C = function() {};
C.c = function() {};
C.prototype.testC = function() {
  C.c();
  C.D.d();
};

// class D
C.D = function() {};
C.D.d = function() {};
C.D.prototype.testD = function() {
  C.c();
  C.D.d();
};
C.c();
C.D.d();
+/

class A {
  static void a() {}
  static void ab() {}
  void testA() {
    a()
    ab()
    A.a()
    A.ab()
  }
}

A.a()
A.ab()

class B is A {
  static over void ab() {}
  void testB() {
    a()
    ab()
    A.a()
    A.ab()
    B.a()
    B.ab()
  }
}

B.a()
B.ab()

class C {
  static void c() {}
  void testC() {
    c()
    D.d()
  }

  class D {
    static void d() {}
    void testD() {
      c()
      d()
    }
  }
}

C.c()
C.D.d()
