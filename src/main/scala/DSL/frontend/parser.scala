package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
import parsley.expr.{precedence, Ops, InfixL}
import parsley.combinator.{sepBy, some, option, many}
import parsley.Parsley.atomic

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, double, identifier, fully, sumKeyword, prodKeyword, maxKeyword, minKeyword, mapKeyword, funcKeyword, lbracket, rbracket, distKeyword, poolKeyword}
import DSL.frontend.AST._

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[String, Program] = {
    this.parser.parse(input) match {
      case Success(p)   => Success(p)
      case Failure(msg) => Failure(msg.toString)
    }
  }

  private def wrapInSumIfNeeded(e: Expr): Expr = e match {
    case _: Sum | _: Prod | _: Max | _: Min | _: MapExpr | _: IfExpr | _: Block => e
    case _ => Sum(e)
  }

  private lazy val parser: Parsley[Program] = fully(program)

  private lazy val program: Parsley[Program] =
    some(topLevelItem <~ option(";")).map(Program.apply)

  private lazy val topLevelItem: Parsley[Either[Stmt, Expr]] =
    funcDecl.map(Left(_)) <|> assignStmt.map(Left(_)) <|> expr.map(e => Right(wrapInSumIfNeeded(e)))

  private lazy val assignStmt: Parsley[Assign] =
    atomic((identifier <~ "=") <~> expr).map { case (id, e) => Assign(id, e) }

  private lazy val typ: Parsley[Type] =
    (distKeyword #> DistType) <|> (poolKeyword #> PoolType)

  // Updated to make typing optional
  // Matches "id" or "id: type"
  private lazy val param: Parsley[Param] =
    (identifier <~> option(":" ~> typ)).map { case (name, tOpt) =>
      Param(name, tOpt)
    }

  private lazy val funcDecl: Parsley[Func] =
    atomic(funcKeyword ~> identifier <~ "(").flatMap { name =>
      (sepBy(param, ",") <~ ")" <~> block).map { case (params, body) =>
        Func(name, params, body)
      }
    }

  private lazy val block: Parsley[Block] = 
    ("{" ~> many(blockItem <~ option(";")) <~ "}").map { items =>
      if (items.isEmpty) {
        Block(Nil, IntLiteral(0))
      } else {
        val init = items.dropRight(1).collect { case s: Stmt => s }
        val last = items.last match {
          case e: Expr => e
          case Assign(name, e) => e 
          case Func(_, _, _) => IntLiteral(0) 
        }
        Block(init, last)
      }
    }

  private lazy val blockItem: Parsley[AstNode] =
    funcDecl <|> assignStmt <|> expr

  private val rollOp = char('~')

  private lazy val rollBinding: Parsley[RollBinding] =
    (atomic(identifier <~ "=" <~ rollOp) <~> expr).map { case (id, e) => RollBinding(id, e) }

  private lazy val ifExpr: Parsley[IfExpr] = {
    val ifPart = 
      ("if" ~> many(atomic(rollBinding <~ ";"))).flatMap { binds =>
        expr.flatMap { cond =>
          block.map { body => IfBranch(binds, cond, body) }
        }
      }
      
    val elifPart = 
      ("elif" ~> many(atomic(rollBinding <~ ";"))).flatMap { binds =>
        expr.flatMap { cond =>
          block.map { body => IfBranch(binds, cond, body) }
        }
      }
      
    val elsePart = "else" ~> block
    
    ifPart.flatMap { firstBranch =>
      many(elifPart).flatMap { elifs =>
        elsePart.map { elseBody =>
          IfExpr(firstBranch :: elifs, elseBody)
        }
      }
    }
  }

  private val diceOp = char('d')

  private lazy val expr: Parsley[Expr] = precedence[Expr](term)(
    Ops(InfixL)("==" #> Eq.apply, "<=" #> Le.apply, "<" #> Lt.apply, ">=" #> Ge.apply, ">" #> Gt.apply),
    Ops(InfixL)("+" #> Add.apply, "-" #> Sub.apply, "++" #> PoolConcat.apply)
  )

  private lazy val term: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(diceOp #> Dice.apply),
    Ops(InfixL)("*" #> Mul.apply, "/" #> Div.apply)
  )

  private lazy val poolList: Parsley[Pool] = {
    (lbracket ~> sepBy(term, ",") <~ rbracket).map(Pool.apply)
  }

  private lazy val atom: Parsley[Expr] = 
    literal <|> ifExpr <|> poolList <|> customDistLiteral <|> atomic(sumCall) <|> atomic(prodCall) <|> 
    atomic(maxCall) <|> atomic(minCall) <|> atomic(mapCall) <|> atomic(prefixDice) <|> funcCall <|> identRef <|> parens

  private lazy val literal: Parsley[IntLiteral] = 
    integer.map(IntLiteral.apply)
  
  private lazy val funcCall: Parsley[Call] =
    (atomic(identifier <~ "(") <~> sepBy(expr, ",") <~ ")").map { case (name, args) => Call(name, args) }

  private lazy val identRef: Parsley[Ident] =
    identifier.map(Ident.apply)

  private lazy val customDistLiteral: Parsley[CustomDist] = {
    val entry = integer <~> (":" ~> double)
    ("{" ~> sepBy(entry, ",") <~ "}").map { entries =>
      CustomDist(entries.toMap)
    }
  }

  private lazy val sumCall: Parsley[Sum] = 
    (sumKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Sum.apply)

  private lazy val prodCall: Parsley[Prod] = 
    (prodKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Prod.apply)

  private lazy val maxCall: Parsley[Max] = 
    (maxKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Max.apply)

  private lazy val minCall: Parsley[Min] = 
    (minKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Min.apply)

  private lazy val mapCall: Parsley[MapExpr] = 
    (mapKeyword ~> "(" ~> identifier <~ "," <~> expr <~ ")").map { case (name, e) => MapExpr(name, e) }

  private lazy val prefixDice: Parsley[Dice] = 
    diceOp ~> atom.map(sides => Dice(IntLiteral(1), sides))

  private lazy val parens: Parsley[Expr] = "(" ~> expr <~ ")"
}