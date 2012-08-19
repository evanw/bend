/+
error on line 13: function "needsOver" overrides a function in a base class and must be marked "over"
error on line 15: function "needsNoOver" is marked "over" but doesn't override anything
error on line 16: function "noImpl" cannot be an override without a function body
+/

class A {
  void needsOver() {}
  void hasOver() {}
  void noImpl()
}
class B is A {
  void needsOver() {}
  over void hasOver() {}
  over void needsNoOver() {}
  over void noImpl()
}
