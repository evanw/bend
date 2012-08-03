class Location {
  string? file
  int line

  string toString() {
    if string file = file { return 'in ' + file + ' on line ' + line }
    return 'on line ' + line
  }
}

class Log {
  string[] messages = {}
  bool hasErrors = false

  void warning(Location location, string text) {
    messages.push('warning ' + location + ': ' + text)
  }

  void error(Location location, string text) {
    messages.push('error ' + location + ': ' + text)
    hasErrors = true
  }
}
