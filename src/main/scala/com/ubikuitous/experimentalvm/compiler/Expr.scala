package com.ubikuitous.experimentalvm.compiler

sealed trait Expr
object Expr {
  case class Integer(value : BigInt) extends Expr
  case class Var(name : String) extends Expr
  case class UnaryOperation(op : UnaryOperator, expr : Expr) extends Expr
  case class BinaryOperation(op : BinaryOperator, left : Expr, right : Expr) extends Expr
  case class If(cond : Expr, thenCase : Expr, elseCase : Expr) extends Expr
  case class App(f : Expr, args : List[Expr]) extends Expr
  case class Fun(params : List[String], body : Expr) extends Expr
  case class Definition(name : String, expr : Expr)
  case class Let(definition : Definition, body : Expr) extends Expr
  case class LetRec(definitions : List[Definition], body : Expr) extends Expr
  case class Lazy(expr : Expr) extends Expr  

  sealed trait UnaryOperator 
  case object Neg extends UnaryOperator
  case object Not extends UnaryOperator

  sealed trait BinaryOperator
  case object Add extends BinaryOperator
  case object Sub extends BinaryOperator
  case object Mul extends BinaryOperator
  case object Div extends BinaryOperator
  case object Mod extends BinaryOperator
  case object Eq extends BinaryOperator
  case object NEq extends BinaryOperator
  case object Le extends BinaryOperator
  case object Leq extends BinaryOperator
  case object Gr extends BinaryOperator
  case object Geq extends BinaryOperator  
}

