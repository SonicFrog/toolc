package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ListBuffer

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

    def isOneOf(kind : TokenKind, more : TokenKind*) : Boolean = {
      (kind :: more.toList).contains(currentToken.kind)
    }


    def isTerminal : Boolean = isOneOf(IDKIND, TRUE, FALSE)

    def expr: ExprTree = {
      currentToken.kind match {
        case IDKIND => ???
        case INTLITKIND => ???
        case STRLITKIND => ???
        case BANG => ???
        case TRUE => ???
        case FALSE => ???
        case NEW => ???
        case LPAREN => ???
        case THIS => ???

        case _ => expected(IDKIND, INTLITKIND, STRLITKIND, BANG, TRUE, FALSE)
      }
    }

    def parseType: TypeTree = {
      currentToken.kind match {
        case IDKIND => new Identifier(currentToken.asInstanceOf[ID].value)
        case INT =>
          readToken; currentToken.kind match {
            case RBRACKET =>
              eat(RBRACKET); eat(LBRACKET); new IntArrayType()
            case _ => eat(INT); new IntType()
          }
        case STRING => eat(STRING); new StringType()
        case BOOLEAN => eat(BOOLEAN); new BooleanType()

        case _ => expected(IDKIND, STRING, INT, BOOLEAN)

      }
    }

    def parseClass : ClassDecl = {
      eat(CLASS)

      var parent : Option[Identifier] = None
      var attributes : ListBuffer[VarDecl] = ListBuffer()
      var methods : ListBuffer[MethodDecl] = ListBuffer()

      val methodName = currentToken match {
        case name : ID => new Identifier(name.value)
        case _ => expected(IDKIND)
      }

      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
        val parentName = currentToken match {
          case name : ID => new Identifier(name.value)
          case _ => expected(IDKIND)
        }

        parent = Some(parentName)
      }

      eat(LBRACE)

      while (currentToken.kind == VAR)
        attributes += parseVarDecl

      while (currentToken.kind == DEF)
        methods += parseMethod

      eat(RBRACE)

      new ClassDecl(methodName, parent, attributes.toList, methods.toList)
    }

    def parseObject : MainObject = {
      eat(OBJECT)

      val name = currentToken match {
        case tok : ID => currentToken.asInstanceOf[ID].value
        case _ => expected(IDKIND)
      }

      eat(IDKIND)
      eat(RBRACE)
      eat(DEF)
      eat(MAIN)
      eat(RPAREN)
      eat(LPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(RBRACE)

      var statments : ListBuffer[StatTree] = ListBuffer()

      while(currentToken.kind != LBRACE) statments += statmt

      eat(LBRACE)
      eat(LBRACE)

      new MainObject(new Identifier(name), statments.toList)

    }

    def parseMethod : MethodDecl = {
      eat(DEF)
      val methodID = currentToken match {
        case a : ID => new Identifier(currentToken.asInstanceOf[ID].value)
        case _ => expected(IDKIND)
      }

      var args : ListBuffer[Formal] = ListBuffer()
      var variables : ListBuffer[VarDecl] = ListBuffer()
      var statements : ListBuffer[StatTree] = ListBuffer()

      eat(LPAREN)

      do {
        val id = currentToken match {
          case a : ID => new Identifier(a.value)
          case _ => expected(IDKIND)
        }
        readToken
        eat(COLON)
        val tpe = parseType

        args +=  Formal(tpe, new Identifier(id.value))
      } while (currentToken.kind == COMMA);
      eat(RPAREN)

      eat(COLON)

      val retType = parseType

      eat(EQSIGN)
      eat(LBRACE)

      while(currentToken.kind == VAR) {
        variables += parseVarDecl
      }

      while (currentToken.kind != RETURN) {
        statements += statmt
      }

      eat(RETURN)

      val retExpr = expr
      eat(SEMICOLON)
      eat(RBRACE)

      new MethodDecl(retType, methodID, args.toList, variables.toList,
        statements.toList, retExpr)

    }

    def parseVarDecl : VarDecl = {
      eat(VAR)

      val varId = currentToken match {
        case id: ID => new Identifier(id.value)
        case _ => expected(IDKIND)
      }

      eat(COLON)
      readToken
      val varType = parseType
      eat(SEMICOLON)
      new VarDecl(varType, varId)
    }

    def findExprInParenthesis: ExprTree = {
      eat(LPAREN)
      val e = expr
      eat(RPAREN)
      e
    }

    def statmt: StatTree = {
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
          var statements: List[StatTree] = List()
          while (currentToken.kind != RBRACE) statements = statements :+ statmt
          eat(RBRACE)
          new Block(statements)
        }

        case IDKIND => { //Assignation statement
          val id: ID = currentToken.asInstanceOf[ID]
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

        case _ => expected(IDKIND, LBRACE, IF, WHILE, PRINTLN)
      }
    }

    def parseGoal: Program = {
      val mainObject = parseObject
      var classes : ListBuffer[ClassDecl] = ListBuffer()

      while (currentToken.kind == CLASS)
        classes += parseClass

      eat(EOF)

      new Program(mainObject, classes.toList)
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
