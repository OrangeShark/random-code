import re
from llvm.core import Module, Constant, Type, Function, Builder, FCMP_ULT

# Globals
# The LLVM module, which holds all the IR code.
g_llvm_module = Module.new('my cool jit')

# The LLVM instruction builder. Created whenever a new function is entered.
g_llvm_builder = None

# A dictionary of current scope
g_named_values = {}

# Lexer
class EOFToken(object):
  pass

class DefToken(object):
  pass

class ExternToken(object):
  pass

class IdentifierToken(object):
  def __init__(self, name): self.name = name

class NumberToken(object):
  def __init__(self,value): self.value = value

class CharacterToken(object):
  def __init__(self, char): self.char = char
  def __eq__(self, other):
    return isinstance(other, CharacterToken) and self.char == other.char
  def __ne__(self, other): return not self == other

# Regular expressions that match the tokens and comments of our language.
REGEX_NUMBER = re.compile('[0-9]+(?:\.[0-9]+)?')
REGEX_IDENTIFIER = re.compile('[a-zA-Z][a-zA-Z0-9]*')
REGEX_COMMENT = re.compile('#.*')

def Tokenize(string):
  while string:
    #Skip whitespace
    if string[0].isspace():
      string = string[1:]
      continue

    # Run regexes
    comment_match = REGEX_COMMENT.match(string)
    number_match = REGEX_NUMBER.match(string)
    identifier_match = REGEX_IDENTIFIER.match(string)

    # check if matches
    if comment_match:
      comment = comment_match.group(0)
      string = string[len(comment):]
    elif number_match:
      number = number_match.group(0)
      yield NumberToken(float(number))
      string = string[len(number):]
    elif identifier_match:
      identifier = identifier_match.group(0)
      # Check if we matched a keyword
      if identifier == 'def':
        yield DefToken()
      elif identifier == 'extern':
        yield ExternToken()
      else:
        yield IdentifierToken(identifier)
      string = string[len(identifier):]
    else:
      # Yield the unknown character
      yield CharacterToken(string[0])
      string = string[1:]

  yield EOFToken()

# Parser

# Base class for all expression nodes.
class ExpressionNode(object):
  pass

# Expression class for numeric literals like "1.0".
class NumberExpressionNode(ExpressionNode):
  def __init__(self, value):
    self.value = value

  def CodeGen(self):
    return Constant.real(Type.double(), self.value)

# Expression class for referencing a variable, like "a"
class VariableExpressionNode(ExpressionNode):
  def __init__(self, name):
    self.name = name

  def CodeGen(self):
    if self.name in g_named_values:
      return g_named_values[self.name]
    else:
      raise RuntimeError('Unknown variable name: ' + self.name)

# Expression class for a binary operator.
class BinaryOperatorExpressionNode(ExpressionNode):
  def __init__(self, operator, left, right):
    self.operator = operator
    self.left = left
    self.right = right

  def CodeGen(self):
    left = self.left.CodeGen()
    right = self.right.CodeGen()

    if self.operator == '+':
      return g_llvm_builder.fadd(left, right, 'addtmp')
    elif self.operator == '-':
      return g_llvm_builder.fsub(left, right, 'subtmp')
    elif self.operator == '*':
      return g_llvmbuilder.fmul(left, right, 'multmp')
    elif self.operator == '<':
      result = g_llvm_builder.fcmp(FCMP_ULT, left, right, 'cmptmp')
      #Convert bool 0 or 1 to double 0.0 or 1.0
      return g_llvm_builder.uitofp(result, Type.double(), 'booltmp')
    else:
      raise RuntimeError('Unknown binary operator.')

# Expression class for function calls.
class CallExpressionNode(ExpressionNode):
  def __init__(self, callee, args):
    self.callee = callee
    self.args = args

  def CodeGen(self):
    # Look up the name in the global module table.
    callee = g_llvm_module.get_function_named(self.callee)

    # Check for argument mismatch error.
    if len(callee.args) != len(self.args):
      raise RuntimeError('Incorrect number of arguments passed.')

    arg_values = [i.CodeGen() for i in self.args]

    return g_llvm_builder.call(callee, arg_values, 'calltmp')

# This class repersents the "prototype" for a function, which captures its name,
# and its argument names (thus implicitly the number of arguments the function
# takes).
class PrototypeNode(object):
  def __init__(self, name, args):
    self.name = name
    self.args = args

  def CodeGen(self):
    # Make the function type, eg. double(double, double).
    funct_type = Type.function(
        Type.double(), [Type.double()] * len(self.args), False)

    function = Function.new(g_llvm_module, funct_type, self.name)

    # If the name conflicted, there was already something with the same name.
    # If it has a body, don't allow redefinition or reextern.
    if function.name != self.name:
      function.delete()
      function = g_llvm_module.get_function_named(self.name)

    # If the function already has a body, reject this.
    if not function.is_declaration:
      raise RuntimeError('Redefinition of function.')

    # If F took a different number of args, reject.
    if len(callee.args) != len(self.args):
      raise RuntimeError('Redeclaration of a function with a different number of args.')

    # Set names for all arguments and add them to the variables symbol table.
    for arg, arg_name in zip(function.args, self.args):
      arg.name = arg_name
      # Add arguments to variable symbol table.
      g_named_values[arg_name] = arg

    return function

# This class represents a function definition itself.
class FunctionNode(object):
  def __init__(self, prototype, body):
    self.prototype = prototype
    self.body = body

  def CodeGen(self):
    # Clear scope.
    g_named_values.clear()

    # Create a function object.
    function = self.prototype.CodeGen()

    # Create a new basic block to start insertion into.
    block = function.append_basic_block('entry')
    global g_llvm_builder
    g_llvm_builder = Builder.new(block)

    # Finish off the function.
    try:
      return_value  = self.body.CodeGen()
      g_llvm_builder.ret(return_value)

      # Validate the generated code, checking for consistency.
      function.verify()

    except:
      function.delete()
      raise

    return function

class Parser(object):
  def __init__(self, tokens, binop_precedence):
    self.tokens = tokens
    self.binop_precedence = binop_precedence
    self.Next()

  # Provide a simple token buffer. Parser.current is the current token the
  # parser is looking at. Parser.Next() reads another token from the lexer and
  # updates Parser.current with its results.
  def Next(self):
    self.current = next(self.tokens)

  # numberexpr ::= number
  def ParseNumberExpr(self):
    result = NumberExpressionNode(self.current.value)
    self.Next()
    return result

  # parenexpr ::= '(' expression ')'
  def ParseParenExpr(self):
    self.Next() # eat '('.

    contents = self.ParseExpression()

    if self.current != CharacterToken(')'):
      raise RuntimeError('Expected ")".')
    self.Next() # eat ')'.

    return contents

  # identifierexpr ::= identifier | identifier '(' expression* ')'
  def ParseIdentifierExpr(self):
    identifier_name = self.current.name
    self.Next() # eat identifier.

    if self.current != CharacterToken('('): # Simple variable reference.
      return VariableExpressionNode(identifier_name);

    # Call.
    self.Next() # eat '('.
    args = []
    if self.current != CharacterToken(')'):
      while True:
        args.append(self.ParseExpression())
        if self.current == CharacterToken(')'):
          break
        elif self.current != CharacterToken(','):
          raise RuntimeError('Expected ")" or "," in argument list.')
        self.Next()

    self.Next() # eat ')'.
    return CallExpressionNode(identifier_name, args)

  # primary ::= identifierexpr | numberexpr | parenexpr
  def ParsePrimary(self):
    if isinstance(self.current, IdentifierToken):
      return self.ParseIdentifierExpr()
    elif isinstance(self.current, NumberToken):
      return self.ParseNumberExpr()
    elif self.current == CharacterToken('('):
      return self.ParseParenExpr()
    else:
      raise RuntimeError('Unknown token when expecting an expression.')

  # Gets the precedence of the current token, or -1 if the token is not a binary
  # operator.
  def GetCurrentTokenPrecedence(self):
    if isinstance(self.current, CharacterToken):
      return self.binop_precedence.get(self.current.char, -1)
    else:
      return -1

  # expression ::= primary binoprhs
  def ParseExpression(self):
    left = self.ParsePrimary()
    return self.ParseBinOpRHS(left, 0)

  # binoprhs ::= (operator primary)*
  def ParseBinOpRHS(self, left, left_precedence):
    # If this is a binary operator, find its precedence.
    while True:
      precedence = self.GetCurrentTokenPrecedence()

      # If this is a binary operator that binds at least as tightly as the
      # current one, consume it; otherwise we are done.
      if precedence < left_precedence:
        return left

      binary_operator = self.current.char
      self.Next() # eat the operator.

      # Parse the primary expression after the binary operator.
      right = self.ParsePrimary()

      # If binary_operator binds less tightly with right than the operator after
      # right, let the pending operator take right as its left.
      next_precedence = self.GetCurrentTokenPrecedence()
      if precedence < next_precedence:
        right = self.ParseBinOpRHS(right, precedence + 1)

      left = BinaryOperatorExpressionNode(binary_operator, left, right)


    # prototype ::= id '(' id* ')'
  def ParsePrototype(self):
    if not isinstance(self.current, IdentifierToken):
      raise RuntimeError('Expected function name in prototype.')

    function_name = self.current.name
    self.Next() # eat function name.

    if self.current != CharacterToken('('):
      raise RuntimeError('Expected "(" in prototype.')
    self.Next() # eat '('.

    arg_names = []
    while isinstance(self.current, IdentifierToken):
      arg_names.append(self.current.name)
      self.Next()

    if self.current != CharacterToken(')'):
      raise RuntimeError('Expected ")" in prototype.')

    # Success.
    self.Next() # eat ')'.

    return PrototypeNode(function_name, arg_names)

 # definition ::= 'def' prototype expression
  def ParseDefinition(self):
    self.Next() # eat def.
    proto = self.ParsePrototype()
    body = self.ParseExpression()
    return FunctionNode(proto, body)

  # toplevelexpr ::= expression
  def ParseTopLevelExpr(self):
    proto = PrototypeNode('', [])
    return FunctionNode(proto, self.ParseExpression())

  # external ::= 'extern' prototype
  def ParseExtern(self):
    self.Next() # eat extern.
    return self.ParsePrototype()

  def HandleDefinition(self):
    self.Handle(self.ParseDefinition, 'Parsed a function definition.')
 
  def HandleExtern(self):
    self.Handle(self.ParseExtern, 'Parsed an extern.')

  def HandleTopLevelExpression(self):
    self.Handle(self.ParseTopLevelExpr, 'Parsed a top-level expression.')

  def Handle(self, function, message):
    try:
      function()
      print(message)
    except Exception as e:
      print('Error:', e)
      try:
        self.Next() # Skip for error recovery.
      except:
        pass


def main():
  # Install standard binary operators.
  # 1 is lowest possible precedence. 40 is the highest.
  operator_precedence = {
      '<': 10,
      '+': 20,
      '-': 20,
      '*': 40
      }

  # Run the main "interpreter loop".
  while True:
    try:
      raw = input('ready> ')
    except KeyboardInterrupt:
      return

    parser = Parser(Tokenize(raw), operator_precedence)
    while True:
      # top ::= definition | external | expression | EOF
      if isinstance(parser.current, EOFToken):
        break
      if isinstance(parser.current, DefToken):
        parser.HandleDefinition()
      elif isinstance(parser.current, ExternToken):
        parser.HandleExtern()
      else:
        parser.HandleTopLevelExpression()

if __name__ == '__main__':
  main()
