type token = TokenLeftParen | TokenRightParen | TokenNumber(string) | TokenString(string) | TokenName(string)

let tokenizer = (input: string): array<token> => {
    let current = ref(0)

    let bumpCurrent = () => { current := current.contents + 1 }

    let getCurrentChar = () => Js.String.get(input, current.contents)

    let tokens = []

    let numbersRe = %re("/[0-9]/")
    let lettersRe = %re("/[a-z]/i")

    while current.contents < input -> Js.String2.length {
        let char = ref(getCurrentChar())

        switch char.contents {
            | "(" => {
                tokens -> Js.Array2.push(TokenLeftParen) -> ignore
                bumpCurrent()
            }
            | ")" => {
                tokens -> Js.Array2.push(TokenRightParen) -> ignore
                bumpCurrent()
            }
            | " " | "\n" | "\t" => {
                bumpCurrent()
            }
            | _ if Js.Re.test_(numbersRe, char.contents) => {
                let value = ref("")
                while numbersRe -> Js.Re.test_(char.contents) {
                    value := value.contents ++ char.contents
                    bumpCurrent()
                    char := getCurrentChar()
                }
                tokens -> Js.Array2.push(TokenNumber(value.contents)) -> ignore
            }
            | "\"" => {
                let value = ref("")
                bumpCurrent()
                while char.contents != "\"" {
                    value := value.contents ++ char.contents
                    bumpCurrent()
                    char := getCurrentChar()
                }
                bumpCurrent()
                char := getCurrentChar()

                tokens -> Js.Array2.push(TokenString(value.contents)) -> ignore
            }
            | _ if lettersRe -> Js.Re.test_(char.contents) => {
                let value = ref("")
                while Js.Re.test_(lettersRe, char.contents) {
                    value := value.contents ++ char.contents
                    bumpCurrent()
                    char := getCurrentChar()
                }
                tokens -> Js.Array2.push(TokenName(value.contents)) -> ignore
            }
            | _ => {
                Js.Exn.raiseTypeError(`I dont know what this character is: ${char.contents}`)
            }
        }
    }

    tokens
}

module LispAst = {
    type literal = String(string) | Number(string)
    type rec expression = Call(call_expression) | Literal(literal)
    and call_expression = {
        name: string,
        params: array<expression>,
    }
    type program = {
        body: array<expression>
    }
    
}

module LispAstKind = {
    open LispAst
    type kind = Program(program) | CallExpression(call_expression) | Expression(expression) | Literal(literal)
}


let parser = (tokens: array<token>): LispAst.program => {
    let current = ref(0)
    let bumpCurrent = () => current := current.contents + 1

    let getCurrentToken = () => Js.Array.unsafe_get(tokens, current.contents)

    let rec walk = (): LispAst.expression => {
        let token = ref(getCurrentToken())

        switch token.contents {
            | TokenNumber(value) => {
                bumpCurrent()
                LispAst.Literal(LispAst.Number(value))
            }
            | TokenString(value) => {
                bumpCurrent()
                LispAst.Literal(LispAst.String(value))
            }
            | TokenLeftParen => {
                bumpCurrent()
                token := getCurrentToken()
                let TokenName(name) = token.contents
                let node: LispAst.call_expression = {
                    name,
                    params: []
                }

                bumpCurrent()
                token := getCurrentToken()

                while token.contents != TokenRightParen {
                    let _ = Js.Array.push(walk(), node.params)
                    token := getCurrentToken()
                }

                bumpCurrent()
                LispAst.Call(node)
            }
            | _ => {
                Js.log(token)
                Js_exn.raiseError(`unreachable token`)
            }
        }
    }

    let program: LispAst.program = {
        body: []
    }

    while current.contents < Js.Array.length(tokens) {
       let _ = Js.Array.push(walk(), program.body)
    }

    program
}


module Visit = {
    type rec visitor = {
        program: (visitor, LispAst.program) => (),
        expression: (visitor, LispAst.expression, LispAstKind.kind) => (),
        callExpression: (visitor, LispAst.call_expression, LispAstKind.kind) => (),
        literal: (visitor, LispAst.literal, LispAstKind.kind) => (),
    }

    let visitProgramChildren = (visitor: visitor, node: LispAst.program) => {
        node.body -> Js.Array2.forEach(chlid => {
            visitor -> visitor.expression(chlid, LispAstKind.Program(node))
        })
    }
    let visitExpressionChildren = (visitor: visitor, node: LispAst.expression) => {
        switch node {
            | LispAst.Literal(lit) => {
               visitor -> visitor.literal(lit, LispAstKind.Expression(node))
            }
            | LispAst.Call(callExp) => {
                visitor -> visitor.callExpression(callExp, LispAstKind.Expression(node))
            }
        }
    }
    let visitCallExpressionChildren = (visitor: visitor, node: LispAst.call_expression) => {
        node.params -> Js.Array2.forEach(child => {
            visitor -> visitor.expression(child, LispAstKind.CallExpression(node))
        })
    }

    let defaultVisitor: visitor = {
        program: (visitor, node) => visitor -> visitProgramChildren(node),
        expression: (visitor, node, _parent) => visitor -> visitExpressionChildren(node, ),
        callExpression: (visitor, node, _parent) => visitor -> visitCallExpressionChildren(node),
        literal: (_visitor, _node, _parent) => (),
    }
}

let traverse = (ast: LispAst.program, visitor: Visit.visitor): () => {
    visitor -> visitor.program(ast)
}

module CAst = {
    // C style like AST
    type identifier = Identifier(string)

    type literal = String(string) | Number(string)

    type rec expression = Call(call_expression) | Literal(literal)
    and call_expression = {
        callee: identifier,
        arguments: array<expression>
    }

    type statement = Expression(expression)

    type program = {
        body: array<statement>
    }
}

module CAstKind = {
    type kind = Program(CAst.program) | CallExpression(CAst.call_expression)
}

let transformer = (ast: LispAst.program): CAst.program => {
    let newAst: CAst.program = {
        body: []
    }

    let contextStack = []

    let getCurrentContext = () => {
        let len = contextStack -> Js_array2.length
        contextStack -> Js_array2.unsafe_get(len - 1)
    }

    traverse(ast, {
        ...Visit.defaultVisitor,
        program: (vis, node) => {
            contextStack -> Js_array2.push(CAstKind.Program(newAst)) -> ignore
            vis -> Visit.visitProgramChildren(node)
            contextStack -> Js.Array2.pop -> ignore
        },
        callExpression: (vis, node, _parent) => {
            let expression: CAst.call_expression = {
                callee: CAst.Identifier(node.name),
                arguments: [],
            }
            switch getCurrentContext() {
                | CAstKind.Program(context) => {
                    context.body -> Js_array2.push(CAst.Expression(CAst.Call(expression))) -> ignore
                }
                | CAstKind.CallExpression(context) => {
                    context.arguments -> Js_array2.push(CAst.Call(expression)) -> ignore
                }
            }

            contextStack -> Js_array2.push(CAstKind.CallExpression(expression)) -> ignore
            vis -> Visit.visitCallExpressionChildren(node)
            contextStack -> Js.Array2.pop -> ignore
        },
        literal: (_vis, node, _parent) => {
            switch node {
                | LispAst.Number(v) => {
                    switch getCurrentContext() {
                        | CAstKind.Program(context) => {
                            context.body -> Js_array2.push(CAst.Expression(CAst.Literal(CAst.Number(v)))) -> ignore
                        }
                        | CAstKind.CallExpression(context) => {
                            context.arguments -> Js_array2.push(CAst.Literal(CAst.Number(v))) -> ignore
                        }
                    }
                }
                | LispAst.String(v) => {
                    switch getCurrentContext() {
                        | CAstKind.Program(context) => {
                            context.body -> Js_array2.push(CAst.Expression(CAst.Literal(CAst.String(v)))) -> ignore
                        }
                        | CAstKind.CallExpression(context) => {
                            context.arguments -> Js_array2.push(CAst.Literal(CAst.String(v))) -> ignore
                        }
                    }
                }
            }
        }
    })

    newAst
}

let codeGenerator = (node: CAst.program): string => {

    let emitIdentifier = (node: CAst.identifier) => {
        let CAst.Identifier(name) = node
        name
    }

    let emitLiteral = (node: CAst.literal) => {
        switch node {
            | CAst.Number(v) => v
            | CAst.String(v) => `"${v}"`
        }
    }

    let rec emitExpression = (node: CAst.expression) => {
        switch node {
            | CAst.Call(callExp) => {
                `${callExp.callee -> emitIdentifier}(${callExp.arguments -> Js_array2.map(exp => exp -> emitExpression) -> Js_array2.joinWith(", ")})`
            }
            | CAst.Literal(lit) => lit -> emitLiteral
        }
    }

    let emitStatement = (node: CAst.statement) => {
        switch node {
            | CAst.Expression(exp) => {
                `${exp -> emitExpression};`
            }
        }
    }

    let emitProgram = (node: CAst.program) => {
        node.body -> Js_array2.map(child => emitStatement(child)) -> Js_array2.joinWith("\n")
    }

    node -> emitProgram
}

let compiler = (input: string): string => {
    input
        -> tokenizer
        -> parser
        -> transformer
        -> codeGenerator
}
