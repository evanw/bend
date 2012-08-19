var fs = require('fs');
var bend = require('../lib/bend');

var COLOR_ON = '\x1b[35m';
var COLOR_OFF = '\x1b[0m';

var passed = [];
var failed = [];
function summary() {
  process.stdout.write('\r[' + passed.length + '/' + (passed.length + failed.length) + ']');
}

function run(test) {
  if (test.slice(0, 5) != 'test.' || test.slice(test.length - 5) != '.bend')
    return;

  try {
    // Compile the file
    var data = fs.readFileSync(dir + test, 'utf8');
    var compiler = new bend.Compiler();
    compiler.addFile(null, data);
    var module = compiler.compile();

    // Compute the output
    var expectedOutput = data.split('+/\n\n')[0].split('/+\n')[1];
    var actualOutput = '';
    for (var i = 0; i < compiler.log.messages.length; i++)
      actualOutput += compiler.log.messages[i] + '\n';
    if (module)
      actualOutput += bend.JavaScript.convert(module);

    // Decide pass or fail
    if (actualOutput == expectedOutput) passed.push(test);
    else {
      process.stdout.write('\nfail: ' + COLOR_ON + test + COLOR_OFF + '\n');
      process.stdout.write('------ source code ------\n');
      process.stdout.write(data.split('+/\n\n')[1]);
      process.stdout.write('---- expected output ----\n');
      process.stdout.write(expectedOutput);
      process.stdout.write('----- actual output -----\n');
      process.stdout.write(actualOutput);
      process.stdout.write('-------------------------\n');
      failed.push(test);
    }
  }
  catch (e) {
    process.stdout.write('\nfail: ' + COLOR_ON + test + COLOR_OFF + '\n');
    process.stdout.write('------ source code ------\n');
    process.stdout.write(data.split('+/\n\n')[1]);
    process.stdout.write('------ stack trace ------\n');
    process.stdout.write(e.stack + '\n');
    process.stdout.write('-------------------------\n');
    failed.push(test);
  }
  summary();
}

var dir = __dirname + '/../test/';
fs.readdirSync(dir).map(run);
process.stdout.write('\n');
