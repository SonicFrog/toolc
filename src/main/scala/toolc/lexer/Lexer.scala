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
			var string: String = ""
			
			while (sourceIterator.hasNext && f(sourceIterator.head)) {
				string += sourceIterator.next
			}
	
			string.toList
		}

		def readNextToken(): Token = {
			val separators: List[Char] = "!+-*/<=(){}[];:., \n\t".toList
			val whitespaces: List[Char] = "\n \t".toList
			val allNumbers = "0123456789".toList
			val numbers = "([1-9])".r
			val letters = "([a-zA-Z])".r
			var tokenPos = currentPos

			val keywordToToken = Map("if" -> new Token(IF), "else" -> new Token(ELSE), "new" -> new Token(NEW),
					"while" -> new Token(WHILE), "class" -> new Token(CLASS), "println" -> new Token(PRINTLN),
					"return" -> new Token(RETURN), "String" -> new Token(STRING), "Unit" -> new Token(UNIT),
					"Int" -> new Token(INT), "var" -> new Token(VAR), "main" -> new Token(MAIN), "def" -> new Token(DEF),
					"false" -> new Token(FALSE), "true" -> new Token(TRUE), "this" -> new Token(THIS),
					"Bool" -> new Token(BOOLEAN), "object" -> new Token(OBJECT), "extends" -> new Token(EXTENDS), "length" -> new Token(LENGTH))
			var token: Token = null
			
			if (! sourceIterator.hasNext) {
				token = new Token(EOF)
			} else {
				takeWhile(whitespaces.contains(_))
				tokenPos = currentPos
				if (! sourceIterator.hasNext) {
					token = new Token(EOF)
				} else {
					token = sourceIterator.next match {
						case '&' => {
							if (sourceIterator.head == '&') {
								sourceIterator.next
								new Token(AND)
							} else {
								new Token(BAD)
							}
						}
						case '|' => {
							if (sourceIterator.head == '|') {
								sourceIterator.next
								new Token(OR)
							} else {
								new Token(BAD)
							}
						}
						case '=' => {
							if (sourceIterator.head == '=') { 
								sourceIterator.next;
								new Token(EQUALS)
							} else {
							  new Token(EQSIGN)
							}
						}
						case '/' => {
							if (sourceIterator.head == '/') {
								takeWhile(_ != '\n')
								readNextToken
							}
							else if (sourceIterator.head == '*') {
								sourceIterator.next
								var isClosed = false
								while(sourceIterator.hasNext && !isClosed){
									takeWhile(_ != '*')
									if(sourceIterator.hasNext){
										sourceIterator.next
										if (sourceIterator.hasNext && sourceIterator.head == '/'){
											sourceIterator.next
											isClosed = true
										}
									}
								}
								if(isClosed){
									readNextToken
								} else {
								  new Token(BAD)
								}
							}else {
								new Token(DIV)
							}
						}
						case '+' => new Token(PLUS)
						case '<' => new Token(LESSTHAN)
						case '-' => new Token(MINUS)
						case '*' => new Token(TIMES)
						case '!' => new Token(BANG)
						case '[' => new Token(LBRACKET)
						case ']' => new Token(RBRACKET)
						case '(' => new Token(LPAREN)
						case ')' => new Token(RPAREN)
						case ';' => new Token(SEMICOLON)
						case ':' => new Token(COLON)
						case '{' => new Token(LBRACE)
						case '}' => new Token(RBRACE)
						case '.' => new Token(DOT)
						case ',' => new Token(COMMA)
						
						case letters(c) => {
							val string = c + takeWhile(!separators.contains(_)).mkString 
									keywordToToken.getOrElse(string, new ID(string))
						}
		
						case numbers(d) => new INTLIT((d :: takeWhile(allNumbers.contains(_)).toList).mkString.toInt)
		
						case '"' => new STRLIT(source.takeWhile(_ != '\"').mkString)
		
						case '0' => new INTLIT(0)
		
						case _ => new Token(BAD)
					}
				}
			}
			if(!token.hasPosition)
				token.setPos(tokenPos)
			else
				token
		}

		new Iterator[Token] {
			var nextToken: Token = readNextToken;
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
