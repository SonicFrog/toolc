package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    val sourceIterator = source.toIterator.buffered
    var lastRead: Char = '.'

    import ctx.reporter._

    def currentPos(): Positioned = {
      new Positioned {}.setPos(f, source.pos)
    }

    /**
     * takeWhile implementation which leaves the last unmatched character in the iterator
     */
    def takeWhile(f: Char => Boolean): List[Char] = {
      var string : String = ""
      var currentChar = sourceIterator.head
      
      while(f(currentChar)) {
        string += sourceIterator.next
        currentChar = sourceIterator.head
      }
      
      string.toList
    }

    def readNextToken(): Token = {
      val separators: List[Char] = "!+-*/=(){}[];:., \n".toList
      val whitespaces: List[Char] = "\n ".toList
      val numbers: List[Char] = "0123456789".toList

      val keywordToToken = Map("if" -> new Token(IF), "else" -> new Token(ELSE), "new" -> new Token(NEW),
        "while" -> new Token(WHILE), "class" -> new Token(CLASS), "println" -> new Token(PRINTLN),
        "return" -> new Token(RETURN), "String" -> new Token(STRING), "Unit" -> new Token(UNIT),
        "Int" -> new Token(INT), "var" -> new Token(VAR), "main" -> new Token(MAIN), "def" -> new Token(DEF),
        "false" -> new Token(FALSE), "true" -> new Token(TRUE), "this" -> new Token(THIS), 
        "Bool" -> new Token(BOOLEAN))

      if (!sourceIterator.hasNext) new Token(EOF)
      
      var firstChar = sourceIterator.next

      while (whitespaces.contains(firstChar)) {
        firstChar = sourceIterator.next
      }

      val tokenType = firstChar match {
        case '&' => ???
        case '|' => ???
        case '+' => PLUS
        case '<' => LESSTHAN
        case '-' => MINUS
        case '*' => TIMES
        case '!' => BANG
        case '[' => LBRACKET
        case ']' => RBRACKET
        case '(' => LPAREN
        case ')' => RPAREN
        case ';' => SEMICOLON
        case ':' => COLON
        case '{' => LBRACE
        case '}' => RBRACE
        case '/' => DIV //TODO: Implement comments
        case '.' => DOT
        case ',' => COMMA
        case '=' =>
          if (sourceIterator.head == '=') { sourceIterator.next; EQUALS }
          else EQSIGN

        case _ => BAD
      }

      if (tokenType == BAD) { //We have an identifier, a keyword or a litteral
        var string: String = firstChar.toString
        var nChar: Char = firstChar

        if (firstChar == '\"') { //If the token starts with " it's a string literal
          new STRLIT(source.takeWhile(_ != '\"').mkString)
        } else if (numbers.contains(firstChar)) { //If it starts with a number => integer literal
          val strInt = firstChar + takeWhile(!separators.contains(_)).mkString
          new INTLIT(strInt.toInt)
        } else { //Otherwise consider it an identifier or a keyword
          while (!separators.contains(nChar)){
            string += sourceIterator.next
            nChar = sourceIterator.head
          } 
          
          //Fetch it from the map of keywords and if it is not inside make a new ID 
          keywordToToken.getOrElse(string, new ID(string))
        }
      } else {
        new Token(tokenType)
      }
    }

    new Iterator[Token] {
      var nextToken: Token = readNextToken
      var reachedEnd = false

      def hasNext = {
        nextToken.kind != EOF || !reachedEnd
      }

      def next = {
        val r = nextToken
        nextToken = readNextToken
        if (r.kind == EOF) {
          reachedEnd = true
        }
        r
      }
    }
  }
}
