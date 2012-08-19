class JsNode {}
class JsFile is JsNode { JsNode[] nodes }
class JsBlock is JsNode { JsNode[] nodes }
class JsText is JsNode { string text }
class JsComment is JsNode { string text; bool isStmt }
class JsCall is JsNode { JsNode value; JsNode[] args }
class JsFunc is JsNode { string? name; JsNode[] args; JsNode body }
class JsUnary is JsNode { JsNode value; JsUnaryOp op }
class JsBinary is JsNode { JsNode left; JsBinaryOp op; JsNode right }
class JsTertiary is JsNode { JsNode test; JsNode yes; JsNode no }
class JsVars is JsNode { JsNode[] nodes }
class JsVar is JsNode { string name; JsNode? value }
class JsJump is JsNode { JsJumpKind kind; JsNode? value }
class JsArray is JsNode { JsNode[] nodes }
class JsObj is JsNode { JsNode[] nodes }
class JsProp is JsNode { string name; JsNode value }
class JsIf is JsNode { JsNode test; JsNode yes; JsNode? no }
class JsWhile is JsNode { JsNode test; JsNode body; string? label }
class JsFor is JsNode { JsNode init; JsNode test; JsNode update; JsBlock body; string? label }
class JsSwitch is JsNode { JsNode value; JsNode body }
class JsWith is JsNode { JsNode value; JsNode body }

class JsUnaryOp {
  NEW
  PRE_INC
  PRE_DEC
  POST_INC
  POST_DEC
  NOT
  BITNOT
  POS
  NEG
  TYPEOF
  VOID
  DELETE
}

class JsBinaryOp {
  MEMBER
  INDEX
  MUL
  DIV
  MOD
  ADD
  SUB
  SHL
  SHR
  USHR
  LT
  LTE
  GT
  GTE
  IN
  INSTANCEOF
  EQ
  NEQ
  SEQ
  SNEQ
  BITAND
  BITXOR
  BITOR
  AND
  OR
  ASSIGN
  ADD_ASSIGN
  SUB_ASSIGN
  MUL_ASSIGN
  DIV_ASSIGN
  MOD_ASSIGN
  SHL_ASSIGN
  SHR_ASSIGN
  USHR_ASSIGN
  BITAND_ASSIGN
  BITXOR_ASSIGN
  BITOR_ASSIGN
  COMMA
}

class JsJumpKind {
  RETURN
  THROW
  CONTINUE
  BREAK
  CASE
  DEFAULT
}

class JsUnaryOpInfo {
  JsUnaryOp op
  int precedence
  string template

  static JsUnaryOpInfo[] info = {
    { JsUnaryOp.NEW, 1, 'new {0}' }
    { JsUnaryOp.PRE_INC, 3, '++{0}' }
    { JsUnaryOp.PRE_DEC, 3, '--{0}' }
    { JsUnaryOp.POST_INC, 3, '{0}++' }
    { JsUnaryOp.POST_DEC, 3, '{0}--' }
    { JsUnaryOp.NOT, 4, '!{0}' }
    { JsUnaryOp.BITNOT, 4, '~{0}' }
    { JsUnaryOp.POS, 4, '+{0}' }
    { JsUnaryOp.NEG, 4, '-{0}' }
    { JsUnaryOp.TYPEOF, 4, 'typeof {0}' }
    { JsUnaryOp.VOID, 4, 'void {0}' }
    { JsUnaryOp.DELETE, 4, 'delete {0}' }
  }
}

class JsBinaryOpInfo {
  JsBinaryOp op
  int precedence
  string template

  static JsBinaryOpInfo[] info = {
    { JsBinaryOp.MEMBER, 1, '{0}.{1}' }
    { JsBinaryOp.INDEX, 1, '{0}[{1}]' }
    { JsBinaryOp.MUL, 5, '{0} * {1}' }
    { JsBinaryOp.DIV, 5, '{0} / {1}' }
    { JsBinaryOp.MOD, 5, '{0} % {1}' }
    { JsBinaryOp.ADD, 6, '{0} + {1}' }
    { JsBinaryOp.SUB, 6, '{0} - {1}' }
    { JsBinaryOp.SHL, 7, '{0} << {1}' }
    { JsBinaryOp.SHR, 7, '{0} >> {1}' }
    { JsBinaryOp.USHR, 7, '{0} >>> {1}' }
    { JsBinaryOp.LT, 8, '{0} < {1}' }
    { JsBinaryOp.LTE, 8, '{0} <= {1}' }
    { JsBinaryOp.GT, 8, '{0} > {1}' }
    { JsBinaryOp.GTE, 8, '{0} >= {1}' }
    { JsBinaryOp.IN, 8, '{0} in {1}' }
    { JsBinaryOp.INSTANCEOF, 8, '{0} instanceof {1}' }
    { JsBinaryOp.EQ, 9, '{0} == {1}' }
    { JsBinaryOp.NEQ, 9, '{0} != {1}' }
    { JsBinaryOp.SEQ, 9, '{0} === {1}' }
    { JsBinaryOp.SNEQ, 9, '{0} !== {1}' }
    { JsBinaryOp.BITAND, 10, '{0} & {1}' }
    { JsBinaryOp.BITXOR, 11, '{0} ^ {1}' }
    { JsBinaryOp.BITOR, 12, '{0} | {1}' }
    { JsBinaryOp.AND, 13, '{0} && {1}' }
    { JsBinaryOp.OR, 14, '{0} || {1}' }
    { JsBinaryOp.ASSIGN, 16, '{0} = {1}' }
    { JsBinaryOp.ADD_ASSIGN, 16, '{0} += {1}' }
    { JsBinaryOp.SUB_ASSIGN, 16, '{0} -= {1}' }
    { JsBinaryOp.MUL_ASSIGN, 16, '{0} *= {1}' }
    { JsBinaryOp.DIV_ASSIGN, 16, '{0} /= {1}' }
    { JsBinaryOp.MOD_ASSIGN, 16, '{0} %= {1}' }
    { JsBinaryOp.SHL_ASSIGN, 16, '{0} <<= {1}' }
    { JsBinaryOp.SHR_ASSIGN, 16, '{0} >>= {1}' }
    { JsBinaryOp.USHR_ASSIGN, 16, '{0} >>>= {1}' }
    { JsBinaryOp.BITAND_ASSIGN, 16, '{0} &= {1}' }
    { JsBinaryOp.BITXOR_ASSIGN, 16, '{0} ^= {1}' }
    { JsBinaryOp.BITOR_ASSIGN, 16, '{0} |= {1}' }
    { JsBinaryOp.COMMA, 17, '{0}, {1}' }
  }
}

class JsPrinter {
  string spaces = ''

  static JsBinaryOp[] rightAssociative = {
    JsBinaryOp.ASSIGN
    JsBinaryOp.ADD_ASSIGN
    JsBinaryOp.SUB_ASSIGN
    JsBinaryOp.MUL_ASSIGN
    JsBinaryOp.DIV_ASSIGN
    JsBinaryOp.MOD_ASSIGN
    JsBinaryOp.SHL_ASSIGN
    JsBinaryOp.SHR_ASSIGN
    JsBinaryOp.USHR_ASSIGN
    JsBinaryOp.BITAND_ASSIGN
    JsBinaryOp.BITXOR_ASSIGN
    JsBinaryOp.BITOR_ASSIGN
  }

  static int precedence(JsNode node) {
    if JsUnary unary = node {
      for info in JsUnaryOpInfo.info {
        if info.op == unary.op { return info.precedence }
      }
      fail
    }
    else if JsBinary binary = node {
      for info in JsBinaryOpInfo.info {
        if info.op == binary.op { return info.precedence }
      }
      fail
    }
    else if node is JsCall { return 1 } // This is technically 2, but causes unnecessary parentheses in that case
    else if node is JsTertiary { return 15 }
    else { return 0 }
  }

  string visitStmts(JsNode[] stmts) {
    string text = ''
    bool wasComment = false
    bool wasCommentStmt = false
    for stmt in stmts {
      bool isComment = stmt is JsComment
      bool isNamedFunc = JsFunc func = stmt and func.name != null
      bool isBlockStmt = stmt is JsIf or stmt is JsWhile or stmt is JsFor or stmt is JsSwitch or stmt is JsWith
      bool needsColon = JsJump jump = stmt and (jump.kind == JsJumpKind.CASE or jump.kind == JsJumpKind.DEFAULT)
      string printed = visit(stmt)
      if printed.indexOf('{') == 0 or ((printed.indexOf('function ') == 0 or printed.indexOf('function(') == 0) and not isNamedFunc) {
        printed = '(' + printed + ')'
      }
      if (isComment and not wasComment and text.length > 0) or wasCommentStmt {
        text += '\n'
      }
      wasComment = isComment
      wasCommentStmt = JsComment stmt = stmt and stmt.isStmt
      if needsColon {
        text += spaces.slice(2) + printed + ':\n'
        continue
      }
      text += spaces + printed
      if isComment or isNamedFunc or isBlockStmt { text += '\n' }
      else { text += ';\n' }
    }
    return text
  }

  string visit(JsNode node) {
    if JsFile node = node { return visitStmts(node.nodes) }
    else if JsBlock node = node {
      spaces += '  '
      string text = visitStmts(node.nodes)
      spaces = spaces.slice(2)
      return '{\n' + text + spaces + '}'
    }
    else if JsText node = node { return node.text }
    else if JsComment node = node {
      // Remove any trailing newline
      string text = node.text
      if text.length > 0 and text[text.length - 1] == '\n' {
        text = text.slice(0, text.length - 1)
      }
      return text.split('\n').map(line -> '//' + line).join('\n' + spaces)
    }
    else if JsCall node = node {
      string[] args = {}
      for arg in node.args { args.push(visit(arg)) }
      return visit(node.value) + '(' + args.join(', ') + ')'
    }
    else if JsFunc node = node {
      string text = 'function'
      if string name = node.name { text += ' ' + name }
      string[] args = {}
      for arg in node.args { args.push(visit(arg)) }
      text += '(' + args.join(', ') + ') '
      if JsBlock body = node.body and body.nodes.length == 0 { text += '{}' }
      else { text += visit(node.body) }
      return text
    }
    else if JsUnary node = node {
      for info in JsUnaryOpInfo.info {
        if info.op == node.op {
          string text = visit(node.value)
          if precedence(node.value) > precedence(node) { text = '(' + text + ')' }
          return info.template.replace('{0}', text)
        }
      }
      fail
    }
    else if JsBinary node = node {
      string left = visit(node.left)
      string right = visit(node.right)
      bool needLeft
      bool needRight
      if node.op in rightAssociative {
        needLeft = precedence(node.left) >= precedence(node)
        needRight = precedence(node.right) > precedence(node)
      }
      else {
        needLeft = precedence(node.left) > precedence(node)
        needRight = precedence(node.right) >= precedence(node)
      }
      if needLeft { left = '(' + left + ')' }
      if needRight and node.op != JsBinaryOp.INDEX { right = '(' + right + ')' }

      // If conversion to number doesn't result in NaN, make sure it has an extra dot.
      // "100.toHex()" in Bend must be called as "100..toString(16)" in JavaScript.
      if node.op == JsBinaryOp.MEMBER and left.toFloat() == left.toFloat() and '.' not in left { left += '.' }

      for info in JsBinaryOpInfo.info {
        if info.op == node.op {
          // Hack to avoid replacing "{1}" in the text already replaced for "{0}"
          string text = info.template.replace('{1}', '\x01')
          return text.replace('{0}', left).replace('\x01', right)
        }
      }
      fail
    }
    else if JsTertiary node = node {
      return visit(node.test) + ' ? ' + visit(node.yes) + ' : ' + visit(node.no)
    }
    else if JsVars node = node {
      string[] nodes = {}
      for node in node.nodes { nodes.push(visit(node)) }
      return 'var ' + nodes.join(', ')
    }
    else if JsVar node = node {
      string text = node.name
      if JsNode value = node.value { text += ' = ' + visit(value) }
      return text
    }
    else if JsJump node = node {
      string postfix = ''
      if JsNode value = node.value { postfix = ' ' + visit(value) }
      if node.kind == JsJumpKind.CONTINUE { return 'continue' + postfix }
      else if node.kind == JsJumpKind.BREAK { return 'break' + postfix }
      else if node.kind == JsJumpKind.RETURN { return 'return' + postfix }
      else if node.kind == JsJumpKind.THROW { return 'throw' + postfix }
      else if node.kind == JsJumpKind.CASE { return 'case' + postfix }
      else if node.kind == JsJumpKind.DEFAULT { return 'default' + postfix }
      else { fail }
    }
    else if JsArray node = node {
      if node.nodes.length == 0 { return '[]' }
      if node.nodes.length == 1 { return '[' + visit(node.nodes[0]) + ']' }
      spaces += '  '
      int totalSize = 0
      bool newlines = false
      string[] texts = {}
      for node in node.nodes {
        string text = visit(node)
        if '\n' in text { newlines = true }
        totalSize += text.length
        texts.push(text)
      }
      if not newlines and totalSize < 32 {
        string text = texts.join(', ')
        spaces = spaces.slice(2)
        return '[' + text + ']'
      }
      string text = texts.map(text -> spaces + text).join(',\n')
      spaces = spaces.slice(2)
      return '[\n' + text + '\n' + spaces + ']'
    }
    else if JsObj node = node {
      if node.nodes.length == 0 { return '{}' }
      if node.nodes.length == 1 { return '{ ' + visit(node.nodes[0]) + ' }' }
      spaces += '  '
      string[] texts = {}
      for node in node.nodes { texts.push(spaces + visit(node)) }
      string text = texts.join(',\n')
      spaces = spaces.slice(2)
      return '{\n' + text + '\n' + spaces + '}'
    }
    else if JsProp node = node {
      return node.name + ': ' + visit(node.value)
    }
    else if JsIf node = node {
      string postfix = ''
      if JsNode no = node.no { postfix = ' else ' + visit(no) }
      return 'if (' + visit(node.test) + ') ' + visit(node.yes) + postfix
    }
    else if JsWhile node = node {
      string prefix = ''
      if string label = node.label { prefix = label + ':\n' + spaces }
      return prefix + 'while (' + visit(node.test) + ') ' + visit(node.body)
    }
    else if JsFor node = node {
      string prefix = ''
      if string label = node.label { prefix = label + ':\n' + spaces }
      return prefix + 'for (' + visit(node.init) + '; ' + visit(node.test) + '; ' + visit(node.update) + ') ' + visit(node.body)
    }
    else if JsSwitch node = node {
      return 'switch (' + visit(node.value) + ') ' + visit(node.body)
    }
    else if JsWith node = node {
      return 'with (' + visit(node.value) + ') ' + visit(node.body)
    }
    else { fail }
  }
}
