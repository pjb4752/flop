package flop.reading

import scala.collection.immutable.ListMap

object Reading {

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

    read0(input, List[Form]()).reverse
  }

  def read(input: String): List[Form] = read(input.toList)

  private val whitespace = List(' ', '\t', '\n')
  private val digits = List('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
  private val symbolStartChars = ('a' to 'z').toList ++ ('A' to 'Z').toList ++
      List('_', '+', '-', '*', '/', '>', '<', '=')
  private val symbolChars = symbolStartChars ++ digits ++ List('!', '?', '.', '/')

  private def tryRead(input: List[Char]): (List[Char], Form) = {
    val char = input.head

    if (isDigit(char)) {
      readNum(input)
    } else if (isStringDelim(char)) {
      readStr(input)
    } else if (isSymbolStartChar(char)) {
      readSym(input)
    } else if (isListOpen(char)) {
      readList(input)
    } else if (isMapOpen(char)) {
      readMap(input)
    } else {
      throw SyntaxError(s"invalid form starting with: ${char}")
    }
  }

  private def isBlank(char: Char): Boolean = whitespace.contains(char)

  private def isDigit(char: Char): Boolean = digits.contains(char)

  private def isDigit(maybeChar: Option[Char]): Boolean =
    maybeChar.map(isDigit).getOrElse(false)

  private def isStringDelim(char: Char): Boolean = char == '"'

  private def isSymbolStartChar(char: Char): Boolean =
    symbolStartChars.contains(char)

  private def isSymbolChar(char: Char): Boolean =
    symbolChars.contains(char)

  private def isListOpen(char: Char): Boolean = char == '('

  private def isListClose(char: Char): Boolean = char == ')'

  private def isMapOpen(char: Char): Boolean = char == '{'

  private def isMapClose(char: Char): Boolean = char == '}'

  @scala.annotation.tailrec
  private def ignoreBlank(input: List[Char]): List[Char] = {
    if (input.isEmpty || !isBlank(input.head)) {
      input
    } else {
      ignoreBlank(input.tail)
    }
  }

  private type NumResult = Tuple2[List[Char], Form.NumF]
  private def readNum(input: List[Char]): NumResult = {
    @scala.annotation.tailrec
    def readNum0(input: List[Char], output: String): NumResult = {
      if (input.isEmpty || !isDigit(input.head)) {
        val number = output.toFloat
        (input, Form.NumF(number))
      } else {
        readNum0(input.tail, output + input.head)
      }
    }

    readNum0(input, "")
  }

  private type StrResult = Tuple2[List[Char], Form.StrF]
  private def readStr(input: List[Char]): StrResult = {
    @scala.annotation.tailrec
    def readStr0(input: List[Char], output: String): StrResult = {
      if (input.isEmpty) {
        throw SyntaxError("unexpected EOF, expecting '\"'")
      } else if (isStringDelim(input.head)) {
        (input.tail, Form.StrF(output)) // chop off closing quote
      } else {
        readStr0(input.tail, output + input.head)
      }
    }

    readStr0(input.tail, "") // chop off opening quote
  }

  private type SymResult = Tuple2[List[Char], Form.SymF]
  private def readSym(input: List[Char]): SymResult = {
    @scala.annotation.tailrec
    def readSym0(input: List[Char], output: String): SymResult = {
      if (input.isEmpty || !isSymbolChar(input.head)) {
        (input, Form.SymF(output))
      } else {
        readSym0(input.tail, output + input.head)
      }
    }

    readSym0(input, "")
  }

  private type ListResult = Tuple2[List[Char], Form.ListF]
  private def readList(input: List[Char]): ListResult = {
    @scala.annotation.tailrec
    def readList0(input: List[Char], output: List[Form]): ListResult = {
      if (input.isEmpty) {
        throw SyntaxError("unexpected EOF, expecting ')'")
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

  private type MapResult = Tuple2[List[Char], Form.MapF]
  private def readMap(input: List[Char]): MapResult = {
    @scala.annotation.tailrec
    def readMap0(input: List[Char], output: List[Form]): MapResult = {
      if (input.isEmpty) {
        throw SyntaxError("unexpected EOF, expecting '}'")
      } else if (isMapClose(input.head)) {
        if (output.length % 2 == 0) {
          val result = output.grouped(2).foldRight(ListMap[Form, Form]())(
            (el, memo) => memo + (el(1), el(0)))
          (input.tail, Form.MapF(result)) // chop off closing bracket
        } else {
          throw SyntaxError("uneven number of forms in map literal")
        }
      } else if (isBlank(input.head)) {
        val in = ignoreBlank(input)
        readMap0(in, output)
      } else {
        val (in, form) = tryRead(input)
        readMap0(in, form :: output)
      }
    }

    readMap0(input.tail, List[Form]()) // chop off opening bracket
  }
}
