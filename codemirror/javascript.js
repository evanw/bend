CodeMirror.defineMIME('text/javascript', 'javascript');
CodeMirror.defineMode('javascript', function() {
  return {
    startState: function() {
      return { stringChar: null, inComment: 0 };
    },
    token: function(stream, state) {
      if (stream.eatSpace()) {
        return null;
      }

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
      if (stream.match(/^(break|case|catch|continue|debugger|default|delete|do|else|finally|for|function|if|in|instanceof|new|return|switch|throw|try|typeof|var|void|while|with)\b/)) {
        return 'keyword';
      }
      if (stream.match(/^(true|false|null|this|[A-Z][A-Z_]+)\b/)) {
        return 'atom';
      }
      if (stream.match(/^(var|[A-Z][A-Za-z0-9_]+)\b(\[\]|\?)*/)) {
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
        return 'normal';
      }
      if (stream.match(/^[0-9]+(\.[0-0]+)?/)) {
        return 'number';
      }
      if (stream.match(/^[()\[\]{},;:.+\-*/%<>|&^~!=]/)) {
        return 'operator';
      }
      stream.next();
      return 'error';
    },
  };
});
