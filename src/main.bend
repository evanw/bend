// External declarations for node.js
extern var module, process, eval, require, exports

// A command-line compiler driver for node.js
void main() {
  string USAGE = '
Usage:
  bend [inputs] [-o output]

Options:
  -h, --help         Print this help text
  -p, --print-code   Print the compiled output to stdout without evaluating it
  -o, --output-file  Store the output in the file provided by the next argument

Note:
  Without -p or -o, bend will run your code using node
'
  string[] files = {}
  bool flagRun = true
  bool flagPrintCode = false
  bool flagOutputFile = false
  string? outputPath = null

  // Parse command-line arguments
  for arg in process.argv.slice(2) {
    if arg.size == 0 { continue }
    else if flagOutputFile {
      outputPath = arg
      flagOutputFile = false
    }
    else if arg[0] == '-' {
      if arg in { '-h', '--help' } {
        process.stdout.write(USAGE + '\n')
        process.exit(0)
      }
      else if arg in { '-p', '--print-code' } {
        flagPrintCode = true
        flagRun = false
      }
      else if arg in { '-o', '--output-file' } {
        flagOutputFile = true
        flagRun = false
      }
      else {
        process.stdout.write('\nUnknown flag:\n  ' + arg + '\n')
        process.stdout.write(USAGE + '\n')
        process.exit(1)
      }
    }
    else { files.push(arg) }
  }
  if files.size == 0 or flagOutputFile {
    process.stdout.write(USAGE + '\n')
    process.exit(1)
  }

  // Run the compiler
  var fs = require('fs')
  Compiler compiler = {}
  for file in files {
    compiler.addFile(file, fs.readFileSync(file, 'utf8'))
  }
  Module? node = compiler.compile()
  for message in compiler.log.messages {
    process.stdout.write(message + '\n')
  }

  // Use the compiled results
  if Module node = node {
    string code = JavaScript.convert(node)
    if string outputPath = outputPath {
      fs.writeFile(outputPath, code, err -> {
        if err != null {
          process.stdout.write('error: could not write to "' + outputPath + '"\n')
          process.exit(1)
        }
      })
    }
    if flagPrintCode { process.stdout.write(code) }
    if flagRun { eval(code) }
  }
  else { process.exit(1) }
}

// Export the compiler so this module can be used with require()
exports.Compiler = Compiler
exports.main = main

// Run main() if we weren't included via require()
if module.parent == null { main() }
