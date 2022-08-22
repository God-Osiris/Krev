#######################################
# IMPORTS
#######################################

from stringWithArrows import *
import string

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
ALPHANUM = LETTERS + DIGITS

#######################################
# ERRORS
#######################################

class Error:
    def __init__(self, posStart, posEnd, errorName, details):
        self.posStart = posStart
        self.posEnd = posEnd
        self.errorName = errorName
        self.details = details

    def asString(self):
        result = f'{self.errorName}: {self.details}\n'
        result += f'File {self.posStart.fn}, line {self.posStart.ln + 1}'
        result += '\n\n' + stringWithArrows(self.posStart.ftxt, self.posStart, self.posEnd)
        return result

class IllegalCharError(Error):
    def __init__(self, posStart, posEnd, details):
        super().__init__(posStart, posEnd, 'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, posStart, posEnd, details=''):
        super().__init__(posStart, posEnd, 'Invalid Syntax', details)

class RTError(Error):
    def __init__(self, posStart, posEnd, details, context):
        super().__init__(posStart, posEnd, 'Runtime Error', details)
        self.context = context

    def asString(self):
        result = self.generateTraceback()
        result += f'{self.errorName}: {self.details}'
        result += '\n\n' + stringWithArrows(self.posStart.ftxt, self.posStart, self.posEnd)
        return result

    def generateTraceback(self):
        result = ''
        pos = self.posStart
        ctx = self.context

        while ctx:
            result = f'\tFile {pos.fn}, line {str(pos.ln + 1)}, in {ctx.displayName}\n' + result
            pos = ctx.parentEntryPos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result

#######################################
# POSITION
#######################################

class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def proceed(self, currentChar=None):
        self.idx += 1
        self.col += 1
        if currentChar == '\n':
            self.ln += 1
            self.col = 0
        
        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#######################################
# TOKENS
#######################################

KR_INT	        = 'INT'
KR_FLOAT        = 'FLOAT'
KR_IDENTIFIER   = 'IDENTIFIER'
KR_KEYWORD      = 'KEYWORD'
KR_EQ           = 'EQ'
KR_PLUS         = 'PLUS'
KR_MINUS        = 'MINUS'
KR_PROD         = 'PROD'
KR_DIV          = 'DIV'
KR_REM          = 'REM'
KR_POW          = 'POW'
KR_LPAREN       = 'LPAREN'
KR_RPAREN       = 'RPAREN'
KR_EOF		    = 'EOF'

KEYWORDS = [
    'assign'
]

class Token:
    def __init__(self, type_, value=None, posStart=None, posEnd=None):
        self.type = type_
        self.value = value

        if posStart:
            self.posStart = posStart.copy()
            self.posEnd = posStart.copy()
            self.posEnd.proceed()

        if posEnd:
            self.posEnd = posEnd

    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

#######################################
# LEXER
#######################################

class Lexer:
    def __init__(self, fn, line):
        self.fn = fn
        self.line = line
        self.pos = Position(-1, 0, -1, fn, line)
        self.currentChar = None
        self.proceed()

    def proceed(self):
        self.pos.proceed(self.currentChar)
        self.currentChar = self.line[self.pos.idx] if self.pos.idx < len(self.line) else None

    def makeTokens(self):
        tokens = []
        while self.currentChar != None:
            if self.currentChar in ' \t':
                self.proceed()
            elif self.currentChar in DIGITS:
                tokens.append(self.makeNumber())
            elif self.currentChar in LETTERS:
                tokens.append(self.makeIdentifier())
            elif self.currentChar == '+':
                tokens.append(Token(KR_PLUS, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '-':
                tokens.append(Token(KR_MINUS, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '*':
                tokens.append(Token(KR_PROD, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '/':
                tokens.append(Token(KR_DIV, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '%':
                tokens.append(Token(KR_REM, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '^':
                tokens.append(Token(KR_POW, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '=':
                tokens.append(Token(KR_EQ, posStart=self.pos))
                self.proceed()
            elif self.currentChar == '(':
                tokens.append(Token(KR_LPAREN, posStart=self.pos))
                self.proceed()
            elif self.currentChar == ')':
                tokens.append(Token(KR_RPAREN, posStart=self.pos))
                self.proceed()
            else:
                posStart = self.pos.copy()
                char = self.currentChar
                self.proceed()
                return [], IllegalCharError(posStart, self.pos, "'" + char + "'")

        tokens.append(Token(KR_EOF, posStart=self.pos))
        return tokens, None

    def makeNumber(self):
        numStr = ''
        dotCount = 0
        posStart = self.pos.copy()

        while self.currentChar != None and self.currentChar in DIGITS + '.':
            if self.currentChar == '.':
                if dotCount == 1: break
                dotCount += 1
                numStr += '.'
            else:
                numStr += self.currentChar
            self.proceed()

        if dotCount == 0:
            return Token(KR_INT, int(numStr), posStart, self.pos)
        else:
            return Token(KR_FLOAT, float(numStr), posStart, self.pos)

    def makeIdentifier(self):
        idStr = ''
        posStart = self.pos.copy()

        while self.currentChar != None and self.currentChar in ALPHANUM + '_':
            idStr += self.currentChar
            self.proceed()

        tokType = KR_KEYWORD if idStr in KEYWORDS else KR_IDENTIFIER
        return Token(tokType, idStr, posStart, self.pos)

#######################################
# NODES
#######################################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.posStart = self.tok.posStart
        self.posEnd = self.tok.posEnd

    def __repr__(self):
        return f'{self.tok}'

class VarAccessNode:
    def __init__(self, varNameTok):
        self.varNameTok = varNameTok

        self.posStart = self.varNameTok.posStart
        self.posEnd = self.varNameTok.posEnd

class VarAssignNode:
    def __init__(self, varNameTok, valueNode):
        self.varNameTok = varNameTok
        self.valueNode = valueNode

        self.posStart = self.varNameTok.posStart
        self.posEnd = self.valueNode.posEnd

class BinOpNode:
    def __init__(self, leftNode, opTok, rightNode):
        self.leftNode = leftNode
        self.opTok = opTok
        self.rightNode = rightNode

        self.posStart = self.leftNode.posStart
        self.posEnd = self.rightNode.posEnd

    def __repr__(self):
        return f'({self.leftNode}, {self.opTok}, {self.rightNode})'

class UnaryOpNode:
    def __init__(self, opTok, node):
        self.opTok = opTok
        self.node = node

        self.posStart = self.opTok.posStart
        self.posEnd = node.posEnd

    def __repr__(self):
        return f'({self.opTok}, {self.node})'

#######################################
# PARSE RESULT
####################################### 

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error: self.error = res.error
            return res.node

        return res

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self

#######################################
# PARSER
#######################################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tokIdx = -1
        self.proceed()

    def proceed(self, ):
        self.tokIdx += 1
        if self.tokIdx < len(self.tokens):
            self.currentTok = self.tokens[self.tokIdx]
        return self.currentTok

    def parse(self):
        res = self.expr()
        if not res.error and self.currentTok.type != KR_EOF:
            return res.failure(InvalidSyntaxError(
            self.currentTok.posStart, self.currentTok.posEnd,
            "Expected '+', '-', '*', '%', '^' or '/'"
            ))
        return res

    ###################################

    def factor(self):
        res = ParseResult()
        tok = self.currentTok

        if tok.type in (KR_PLUS, KR_MINUS):
            res.register(self.proceed())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (KR_INT, KR_FLOAT):
            res.register(self.proceed())
            return res.success(NumberNode(tok))

        elif tok.type == KR_IDENTIFIER:
            res.register(self.proceed())
            return res.success(VarAccessNode(tok))

        elif tok.type == KR_LPAREN:
            res.register(self.proceed())
            expr = res.register(self.expr())
            if res.error: return res
            if self.currentTok.type == KR_RPAREN:
                res.register(self.proceed())
                return res.success(expr)
        
            else:
                return res.failure(InvalidSyntaxError(
                self.currentTok.posStart, self.currentTok.posEnd,
                "Expected ')'"
                ))

        return res.failure(InvalidSyntaxError(
            tok.posStart, tok.posEnd,
            "Expected int or float"
            ))

    def term(self):
        return self.binOp(self.factor, (KR_PROD, KR_DIV))

    def expr(self):
        res = ParseResult()
        if self.currentTok.matches(KR_KEYWORD, 'assign'):
            res.register(self.proceed())

            if  self.currentTok.type != KR_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.currentTok.posStart, self.currentTok.posEnd,
                    'Expected identifier'
                ))

            varName = self.currentTok
            res.register(self.proceed())

            if self.currentTok != '=':
                return res.failure(InvalidSyntaxError(
                    self.currentTok.posStart, self.currentTok.posEnd,
                    "Expected '='"
                ))

            res.register(self.proceed())
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(varName, expr))

        return self.binOp(self.term, (KR_PLUS, KR_MINUS, KR_REM, KR_POW))

    ###################################

    def binOp(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.currentTok.type in ops:
            opTok = self.currentTok
            res.register(self.proceed())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, opTok, right)

        return res.success(left)

#######################################
# RUNTIME RESULT
#######################################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error: self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self

#######################################
# VALUES
#######################################

class Number:
    def __init__(self, value):
        self.value = value
        self.setPos()
        self.setContext()

    def setPos(self, posStart=None, posEnd=None):
        self.posStart = posStart
        self.posEnd = posEnd
        return self

    def setContext(self, context=None):
        self.context = context
        return self

    def addedTo(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).setContext(self.context), None

    def subtractedBy(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).setContext(self.context), None

    def multipliedWith(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).setContext(self.context), None

    def dividedBy(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.posStart, other.posEnd,
                    'Division by zero',
                    self.context
                )

        return Number(self.value / other.value).setContext(self.context), None

    def remainderBy(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.posStart, other.posEnd,
                    'Division by zero',
                    self.context
                )

        return Number(self.value % other.value).setContext(self.context), None

    def toThePower(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).setContext(self.context), None

    def __repr__(self):
        return str(self.value)

#######################################
# CONTEXT
#######################################

class Context:
    def __init__(self, displayName, parent=None, parentEntryPos=None):
        self.displayName = displayName
        self.parent = parent
        self.parentEntryPos = parentEntryPos
        self.symbolTable = None

#######################################
# SYMBOL TABLE
#######################################

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self, name):
        del self.symbols[name]

#######################################
# INTERPRETER
#######################################

class Interpreter:
    def visit(self, node, context):
        methodName = f'visit{type(node).__name__}'
        method = getattr(self, methodName, self.noVisitMethod)
        return method(node, context)

    def noVisitMethod(self, node, context):
        raise Exception(f'No visit{type(node).__name__} method defined')

    ###################################

    def visitNumberNode(self, node, context):
        return RTResult().success(
        Number(node.tok.value).setContext(context).setPos(node.posStart, node.posEnd)
        )

    def visitVarAccessNode(self, node, context):
        res = RTResult()
        varName = node.varNameTok.value
        value = context.symbolTable.get(varName)

        if not value:
            return res.failure(RTError(
                node.posStart, node.posEnd,
                f"'{varName} is not defined",
                context
            ))

        return res.success(value)

    def visitVarAssignNode(self, node, context):
        res = RTResult()
        varName = node.varNameTok.value
        value = res.register(self.visit(node.valueNode, context))
        if res.error: return res

        context.symbolTable.set(varName, value)
        return res.success(value)

    def visitBinOpNode(self, node, context):
        res = RTResult()
        left = res.register(self.visit(node.leftNode, context))
        if res.error: return res
        right = res.register(self.visit(node.rightNode, context))
        if res.error: return res

        if node.opTok.type == KR_PLUS:
            result, error = left.addedTo(right)
        elif node.opTok.type == KR_MINUS:
            result, error = left.subtractedBy(right)
        elif node.opTok.type == KR_PROD:
            result, error = left.multipliedWith(right)
        elif node.opTok.type == KR_DIV:
            result, error = left.dividedBy(right)
        elif node.opTok.type == KR_REM:
            result, error = left.remainderBy(right)
        elif node.opTok.type == KR_POW:
            result, error = left.toThePower(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.setPos(node.posStart, node.posEnd))

    def visitUnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res

        error = None

        if node.opTok.type == KR_MINUS:
            number, error = number.multipliedWith(Number(-1))

        if error:
            return res.failure(error)
        else:
            return res.success(number.setPos(node.posStart, node.posEnd))

#######################################
# RUN
#######################################

globalSymbolTable = SymbolTable()
globalSymbolTable.set("null", Number(0))

def run(fn, line):
    # Generate tokens
    lexer = Lexer(fn, line)
    tokens, error = lexer.makeTokens()
    if error: return None, error

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    # Run program
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbolTable = globalSymbolTable
    result = interpreter.visit(ast.node, context)

    return result.value, result.error