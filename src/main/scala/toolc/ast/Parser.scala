package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

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
        case TRUE => ???
        case FALSE => ???

        case _ => expected(IDKIND, INTLITKIND, STRLITKIND, TRUE, FALSE)
      }
    }

    def decl : Tree = {
      currentToken.kind match {
        case DEF => ??? //Declaring a method
        case CLASS => ??? //Declaring a class
        case VAR => ??? //Declaring a var
        case OBJECT => ??? //Declaring an object
        case IDKIND => ??? //Declaring a user defined class

        case _ => expected(DEF, CLASS, VAR, OBJECT)
      }
    }

    def statmt : StatTree = {
      currentToken.kind match {
        case PRINTLN => //Println statement
          readToken
          eat(LPAREN)
          val e = expr
          eat(RPAREN)
          eat(SEMICOLON)

          new Println(e)

        case WHILE => { //While statement
          readToken
          eat(LPAREN)
          val cond = expr
          eat(RPAREN)
          new While(cond, statmt)
        }

        case IF => { //If statement
          readToken
          eat(LPAREN)
          val cond = expr
          eat(RPAREN)
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
