/+
var a = function() {
  return 0;
};
var b = function() {
  if (maybe) {
    return 1;
  }
  return 0;
};
var c = function() {
  if (maybe) {
    return 1;
  } else {
    return 0;
  }
};
+/

extern bool maybe

int a() {
  return 0
}

int b() {
  if maybe { return 1 }
  return 0
}

int c() {
  if maybe { return 1 }
  else { return 0 }
}
