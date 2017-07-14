package flop

object Read {

  case class SyntaxError(val message: String) extends Exception(message)

  val whitespace = List(' ', '\t', '\n')
  val digits = List('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
  val symbolStartChars = ('a' to 'z').toList ++ ('A' to 'Z').toList ++
      List('_', '+', '-', '*', '/', '>', '<', '=')
  val symbolChars = symbolStartChars ++ digits ++ List('!', '?')

  def read(input: List[Char]): List[Form] = {
    @scala.annotation.tailrec
    def read0(input: List[Char], forms: List[Form]): List[Form] = {
      if (input.isEmpty) {
        forms
      } else if (isBlank(input.head)) {
        val newInput = ignoreBlank(input)
        read0(newInput, forms)
      } else {
        val (newInput, newForm) = tryRead(input)
        read0(newInput, newForm :: forms)
      }
    }

    read0(input, List[Form]())
  }

  def tryRead(input: List[Char]): (List[Char], Form) = {
    val char = input.head

    if (isDigit(char)) {
      readNum(input)
    } else if (isStringDelim(char)) {
      readStr(input)
    } else if (isSymbolStartChar(char)) {
      readSym(input)
    } else if (isListOpen(char)) {
      readList(input)
    } else {
      throw new SyntaxError("invalid form starting with: " + char)
    }
  }

  def isBlank(char: Char): Boolean = whitespace.contains(char)

  def isDigit(char: Char): Boolean = digits.contains(char)

  def isDigit(maybeChar: Option[Char]): Boolean =
    !maybeChar.isEmpty && isDigit(maybeChar.get)

  def isStringDelim(char: Char): Boolean = char == '"'

  def isSymbolStartChar(char: Char): Boolean = symbolStartChars.contains(char)

  def isSymbolChar(char: Char): Boolean = symbolChars.contains(char)

  def isListOpen(char: Char): Boolean = char == '('

  def isListClose(char: Char): Boolean = char == ')'

  @scala.annotation.tailrec
  def ignoreBlank(input: List[Char]): List[Char] = {
    if (input.isEmpty || !isBlank(input.head)) {
      input
    } else {
      ignoreBlank(input.tail)
    }
  }

  def readNum(input: List[Char]): (List[Char], Form.NumF) = {
    @scala.annotation.tailrec
    def readNum0(input: List[Char], output: String): (List[Char], Form.NumF) = {
      if (input.isEmpty || !isDigit(input.head)) {
        val number = Integer.parseInt(output)
        (input, Form.NumF(number))
      } else {
        readNum0(input.tail, output + input.head)
      }
    }

    readNum0(input, "")
  }

  def readStr(input: List[Char]): (List[Char], Form.StrF) = {
    @scala.annotation.tailrec
    def readStr0(input: List[Char], output: String): (List[Char], Form.StrF) = {
      if (input.isEmpty) {
        throw SyntaxError("expected \", found EOF")
      } else if (isStringDelim(input.head)) {
        (input.tail, Form.StrF(output)) // chop off closing string
      } else {
        readStr0(input.tail, output + input.head)
      }
    }

    readStr0(input.tail, "") // chop off opening string
  }

  def readSym(input: List[Char]): (List[Char], Form.SymF) = {
    @scala.annotation.tailrec
    def readSym0(input: List[Char], output: String): (List[Char], Form.SymF) = {
      if (input.isEmpty || !isSymbolChar(input.head)) {
        (input, Form.SymF(output))
      } else {
        readSym0(input.tail, output + input.head)
      }
    }

    readSym0(input, "") // chop off opening string
  }

  def readList(input: List[Char]): (List[Char], Form.ListF) = {
    @scala.annotation.tailrec
    def readList0(input: List[Char], output: List[Form]): (List[Char], Form.ListF) = {
      if (input.isEmpty) {
        throw SyntaxError("expected ), found EOF")
      } else if (isListClose(input.head)) {
        (input.tail, Form.ListF(output.reverse)) // chop off closing paren
      } else if (isBlank(input.head)) {
        val in = ignoreBlank(input)
        readList0(in, output)
      } else {
        val (in, form) = tryRead(input)
        readList0(in, form :: output)
      }
    }

    readList0(input.tail, List[Form]()) // chop off opening paren
  }
}
