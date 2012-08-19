CodeMirror.defineMIME('text/javascript', 'javascript');
CodeMirror.defineMode('javascript', function() {
  return {
    startState: function() {
      return { stringChar: null, inComment: 0, expectDefinition: false, expectFunctionArgs: false };
    },
    token: function(stream, state) {
      if (stream.eatSpace()) {
        return null;
      }

      var expectDefinition = state.expectDefinition || state.expectFunctionArgs;
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

      if (state.inComment) {
        for (var c = stream.next(); c != null; c = stream.next()) {
          if (c == '*') {
            c = stream.next();
            if (c == '/') {
              state.inComment = false;
              break;
            }
          }
        }
        return 'comment';
      }

      if (stream.match(/^['"]/)) {
        state.stringChar = stream.current();
        return 'string';
      }
      if (stream.match(/^function\b/)) {
        state.expectFunctionArgs = true;
        return 'keyword';
      }
      if (stream.match(/^(break|case|catch|continue|debugger|default|delete|do|else|finally|for|if|in|instanceof|new|return|switch|throw|try|typeof|void|while|with)\b/)) {
        return 'keyword';
      }
      if (stream.match(/^(true|false|null|this|[A-Z][A-Z_]+)\b/)) {
        return 'atom';
      }
      if (stream.match(/^(var|[A-Z][A-Za-z0-9_]+)\b(\[\]|\?)*/)) {
        if (expectDefinition) return 'variable';
        state.expectDefinition = true;
        return 'variable-2';
      }
      if (stream.match(/^\/\//)) {
        stream.skipToEnd();
        return 'comment';
      }
      if (stream.match(/^\/\*/)) {
        state.inComment = true;
        return 'comment';
      }
      if (stream.match(/^([A-Za-z_$][A-Za-z0-9_$]*)\b/)) {
        return expectDefinition ? 'variable' : 'normal';
      }
      if (stream.match(/^[0-9]+(\.[0-0]+)?/)) {
        return 'number';
      }
      if (stream.match(/^\)/)) {
        state.expectFunctionArgs = false;
        return 'operator';
      }
      if (stream.match(/^[(\[\]{},;:.+\-*\/%<>|&^~!=]/)) {
        return 'operator';
      }
      stream.next();
      return 'error';
    },
  };
});
