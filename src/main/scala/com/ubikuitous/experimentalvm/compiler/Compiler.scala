package com.ubikuitous.experimentalvm.compiler

import com.ubikuitous.experimentalvm.vm._

class Compiler(builder : CodeBuilder) {
  
  import Expr._
  import Instruction._
  import Value._
  
  sealed trait Address { def z : Long }
  case class Local(z : Long) extends Address
  case class Global(z : Long) extends Address
  
  type Env = Map[String, Address]
  
  def compile(expr : Expr) : CodeBlock = {
    val code = builder.allocCodeBlock("Main")
    compile(Map(), 0, code, expr)
    code += HALT
    code
  }
  
  /**
   * compile does result in an INT or a ref to a value which is not a closure
   */
  def compile(env : Env, stacklevel : Long, code : CodeBlock, expr : Expr) {
    expr match {
      case Integer(value) => 
        if (value.isValidLong)
          code += LOADINT(INT(value.toLong))
        else
          code += LOADINTEGER(INTEGER(value))
      case UnaryOperation(op, expr) =>
        compile(env, stacklevel, code, expr)
        op match {
          case Neg => code += NEG
          case Not => code += NOT
        }
      case BinaryOperation(op, left, right) =>
        compile(env, stacklevel, code, right)
        compile(env, stacklevel+1, code, left)
        op match {
          case Add => code += ADD
          case Sub => code += SUB
          case Mul => code += MUL
          case Div => code += DIV
          case Mod => code += MOD
          case Eq => code += EQ
          case NEq => code += NEQ
          case Le => code += LE
          case Leq => code += LEQ
          case Gr => code += GR
          case Geq => code += GEQ
        }
      case If(cond, thenExpr, elseExpr) =>
        compile(env, stacklevel, code, cond)
        var A : CodePointer = null
        var B : CodePointer = null
        code += JUMPZ(A)
        val jump_A = code.size - 1
        compile(env, stacklevel, code, thenExpr)
        code += JUMP(B)
        val jump_B = code.size - 1
        A = code.ptr(code.size)
        compile(env, stacklevel, code, elseExpr)
        B = code.ptr(code.size)
        code.replace(jump_A, JUMPZ(A))
        code.replace(jump_B, JUMP(B))
      case _ => throw new RuntimeException("cannot compile: " + expr)
    }
  }

}

object Compiler {
  
  def compile(expr : Expr) : (ProgramStore, CodeBlock) = {
    val ps = new SimpleProgramStore()
    val compiler = new Compiler(ps)
    val codeblock = compiler.compile(expr)
    ps.goto(codeblock.ptr(0))
    (ps, codeblock)
  }
  
  def run(expr : Expr) : Value = {
    val (programStore, codeBlock) = compile(expr)
    val vm = new SimpleVM(programStore, 1000)
    vm.run
    if (vm.stack.SP != 0) 
      throw new RuntimeException("Wrong VM result state, SP = " + vm.stack.SP)
    else 
      vm.resolve(0)
  }
  
  def check(expr : Expr, expectedValue : Value) {
    val value = run(expr)
    if (value == expectedValue) {
      println("OK: expr = "+expr+", value = "+value)
    } else {
      println("FAIL: expr = "+expr+", value = "+value+", expected value = "+expectedValue)
    }
  }
  
  def bin(op : Expr.BinaryOperator, left : Expr, right : Expr) : Expr =
    Expr.BinaryOperation(op, left, right)    
    
  def un(op : Expr.UnaryOperator, expr : Expr) : Expr =
    Expr.UnaryOperation(op, expr)

  
  def main(args : Array[String]) {
    import Expr._
    import Value._
    import scala.language.implicitConversions
    implicit def int2expr(i : Long) : Expr = Integer(i)
    check(42, INT(42))
    check(bin(Add, 2, If(3, bin(Sub, 4, 5), 0)), INT(1))
  }
    
}


