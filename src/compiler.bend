class Compiler {
  Module module = { { '<all>', 0 }, { { '<all>', 0 } } }
  Token[] tokens = {}
  Log log = {}

  void addFile(string file, string text) {
    // Try to tokenize the file (using a temporary log so we know if there were errors)
    Log fileLog = {}
    Token[] fileTokens = Tokenizer.tokenize(fileLog, file, text)
    log.messages = log.messages.concat(fileLog.messages)
    tokens = tokens.concat(fileTokens)

    // If tokenizing worked, try parsing the file
    if not fileLog.hasErrors {
      if Module fileModule = Parser().parse(log, fileTokens) {
        module.body.stmts = module.body.stmts.concat(fileModule.body.stmts)
      }
    }
    else { log.hasErrors = true }
  }

  Module? compile() {
    Pass[] passes = {
      CreateScopesPass(log)
      ResolveBaseTypesPass(log)
      ResolveSymbolTypesPass(log)
      CheckOverridePass(log)
      GenerateCtorsPass(log)
      ResolveTypesPass(log)
    }

    // Run the passes
    for pass in passes {
      pass.visit(module)
      if log.hasErrors { return null }
    }

    return module
  }
}
