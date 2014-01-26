import re

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
    identifier_match = REGEX_IDENTIFER.match(string)

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
