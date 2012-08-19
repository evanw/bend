/+
TODO: all should fail
+/

extern bool maybe

int a() {
}

int b() {
  if maybe { return 1 }
}

int c() {
  if maybe {}
  else { return 0 }
}
