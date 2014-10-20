package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import toolc.eval.Value

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    // Store the current token, as read from the lexer.
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected.
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def expr : ExprTree = {
      currentToken.kind match {
        case IDKIND => ???
        case INTLITKIND => ???
        case STRLITKIND => ???
        case BANG => ???
        case TRUE => ???
        case FALSE => ???

        case _ => expected(IDKIND, INTLITKIND, STRLITKIND, BANG, TRUE, FALSE)
      }
    }
    
    def parseType : TypeTree = {
      ???
    }

    def decl : Tree = {
      currentToken.kind match {
        case DEF => ??? //Declaring a method
        case CLASS => ??? //Declaring a class
        case VAR => {
          readToken
          val varId = currentToken match {
            case id : ID => new Identifier(id.value) 
            case _ => expected(IDKIND)
          }
          eat(COLON)
          readToken
          val varType = parseType
          eat(SEMICOLON)
          new VarDecl(varType, varId)
        }
        case OBJECT => ??? //Declaring an object

        case _ => expected(DEF, CLASS, VAR, OBJECT)
      }
    }

    def findExprInParenthesis : ExprTree = {
        eat(LPAREN)
        val e = expr
        eat(RPAREN)
        e
    }
    
    def statmt : StatTree = {
      currentToken.kind match {
        case PRINTLN => //Println statement
          readToken
          val toPrint = findExprInParenthesis
          eat(SEMICOLON)
          new Println(toPrint)

        case WHILE => { //While statement
          readToken
          new While(findExprInParenthesis, statmt)
        }

        case IF => { //If statement
          readToken
          val cond = findExprInParenthesis
          val ifstat = statmt
          readToken
          if (currentToken.kind == ELSE) new If(cond, ifstat, Some(statmt))
          else new If(cond, ifstat, None)
        }

        case LBRACE => { //Opening a new block of statements
          var statements : List[StatTree] = List()
          while(currentToken.kind != RBRACE) statements = statements :+ statmt
          eat(RBRACE)
          new Block(statements)
        }

        case IDKIND => { //Assignation statement
          val id : ID = currentToken.asInstanceOf[ID]
          val identifier = new Identifier(id.value)

          readToken
          eat(EQSIGN)

          currentToken.kind match {
            case LBRACKET => {
              val arrayIndex = expr
              eat(LBRACKET)
              val assignExpr = expr
              eat(SEMICOLON)
              new ArrayAssign(identifier, arrayIndex, assignExpr)
            }

            case _ => {
              val assignExpr = expr
              eat(SEMICOLON)
              new Assign(identifier, assignExpr)
            }
          }
        }

        case _ => ??? //Error ?
      }
    }

    def parseGoal: Program = {
      ???
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
