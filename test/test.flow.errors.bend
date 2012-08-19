/+
TODO: a, b, and c should fail
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

int d() {
  fail
}

int e() {
  if maybe { return 1 }
  fail
}

int f() {
  if maybe { fail }
  else { return 0 }
}
