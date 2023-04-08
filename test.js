const {
    tokenizer,
    parser,
    transformer,
    codeGenerator,
    compiler,
  } = require('./src/main.bs');
  const assert = require('assert');
  
  const input  = '(add 2 (subtract 4 2))';
  const output = 'add(2, subtract(4, 2));';
  
  const tokens = [
    { type: 'paren',  value: '('        },
    { type: 'name',   value: 'add'      },
    { type: 'number', value: '2'        },
    { type: 'paren',  value: '('        },
    { type: 'name',   value: 'subtract' },
    { type: 'number', value: '4'        },
    { type: 'number', value: '2'        },
    { type: 'paren',  value: ')'        },
    { type: 'paren',  value: ')'        }
  ];
  
  const ast = {
    type: 'Program',
    body: [{
      type: 'CallExpression',
      name: 'add',
      params: [{
        type: 'NumberLiteral',
        value: '2'
      }, {
        type: 'CallExpression',
        name: 'subtract',
        params: [{
          type: 'NumberLiteral',
          value: '4'
        }, {
          type: 'NumberLiteral',
          value: '2'
        }]
      }]
    }]
  };
  
  const newAst = {
    type: 'Program',
    body: [{
      type: 'ExpressionStatement',
      expression: {
        type: 'CallExpression',
        callee: {
          type: 'Identifier',
          name: 'add'
        },
        arguments: [{
          type: 'NumberLiteral',
          value: '2'
        }, {
          type: 'CallExpression',
          callee: {
            type: 'Identifier',
            name: 'subtract'
          },
          arguments: [{
            type: 'NumberLiteral',
            value: '4'
          }, {
            type: 'NumberLiteral',
            value: '2'
          }]
        }]
      }
    }]
  };
  
//   assert.deepStrictEqual(tokenizer(input), tokens, 'Tokenizer should turn `input` string into `tokens` array');
//   assert.deepStrictEqual(parser(tokens), ast, 'Parser should turn `tokens` array into `ast`');
//   assert.deepStrictEqual(transformer(ast), newAst, 'Transformer should turn `ast` into a `newAst`');
  console.log('tokenizer', tokenizer(input))
  console.log('parser', parser(tokenizer(input)))
  console.log('transformer', transformer(parser(tokenizer(input))))
  console.log('codeGenerator', codeGenerator(transformer(parser(tokenizer(input)))))
  assert.deepStrictEqual(compiler(input), output, 'Compiler should turn `input` into `output`');
  
  console.log('All Passed!');