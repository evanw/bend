CodeMirror.defineMIME('text/x-bend', 'bend');
CodeMirror.defineMode('bend', function() {
  return {
    startState: function() {
      return { stringChar: null, commentDepth: 0, fnDepth: 0, expectDefinition: false };
    },
    token: function(stream, state) {
      if (stream.sol()) {
        state.expectDefinition = false;
      }

      if (stream.eatSpace()) {
        return null;
      }

      var expectDefinition = state.expectDefinition;
      state.expectDefinition = false;

      if (state.stringChar) {
        for (var c = stream.next(); c != null; c = stream.next()) {
          if (c == '\\') {
            stream.next();
          } else if (c == state.stringChar) {
            state.stringChar = null;
            break;
          }
        }
        return 'string';
      }

      if (state.commentDepth) {
        for (var c = stream.next(); c != null; c = stream.next()) {
          if (c == '+') {
            c = stream.next();
            if (c == '/' && --state.commentDepth == 0) break;
          } else if (c == '/') {
            c = stream.next();
            if (c == '+') state.commentDepth++;
          }
        }
        return 'comment';
      }

      if (state.fnDepth) {
        for (var c = stream.next(); c != null; c = stream.next()) {
          if (c == ')' && --state.fnDepth == 0) {
            stream.match(/^(\[\]|\?)*/);
            break;
          }
        }
        state.expectDefinition = true;
        return 'variable-2';
      }

      if (stream.match(/^['"]/)) {
        state.stringChar = stream.current();
        return 'string';
      }
      if (stream.match(/^(static|over|abstract|extern|prop|return|if|else|while|continue|break|fail|and|or|is|in|as|not)\b/)) {
        return 'keyword';
      }
      if (stream.match(/^([A-Za-z0-9_]+|\(\s*([A-Za-z0-9_]+(\s*,\s*[A-Za-z0-9_]+)*)?\s*\))?\s*->/)) {
        return 'def';
      }
      if (stream.match(/^(true|false|null|this|base|[A-Z][A-Z_]+)\b/)) {
        return 'atom';
      }
      if (stream.match(/^(var|void|int|bool|float|string|[A-Z][A-Za-z0-9_]+)\b(\[\]|\?)*/)) {
        if (expectDefinition) return 'variable';
        state.expectDefinition = true;
        return 'variable-2';
      }
      if (stream.match(/^fn\(/)) {
        state.fnDepth++;
        return 'variable-2';
      }
      if (stream.match(/^(class|for)\b/)) {
        state.expectDefinition = true;
        return 'keyword';
      }
      if (stream.match(/^\/\//)) {
        stream.skipToEnd();
        return 'comment';
      }
      if (stream.match(/^\/\+/)) {
        state.commentDepth++;
        return 'comment';
      }
      if (stream.match(/^([A-Za-z_][A-Za-z0-9_]*)\b/)) {
        return expectDefinition ? 'variable' : 'normal';
      }
      if (stream.match(/^[0-9]+(\.[0-0]+)?/)) {
        return 'number';
      }
      if (stream.match(/^[()\[\]{},;.?+\-*/%<>|&^~!=]/)) {
        return 'operator';
      }
      stream.next();
      return 'error';
    },
  };
});
