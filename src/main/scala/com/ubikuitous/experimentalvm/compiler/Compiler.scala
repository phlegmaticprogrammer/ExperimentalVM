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
  
  def compile(env : Env, stacklevel : Long, code : CodeBlock, expr : Expr) {
    expr match {
      case Integer(value) => 
        if (value.isValidLong)
          code += Instruction.LOADINT(INT(value.toLong))
        else
          code += Instruction.LOADINTEGER(INTEGER(value))
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
  
  def main(args : Array[String]) {
    import Expr._
    val expr = Integer(42)
    val value = run(expr)
    println("value = " + value)
  }
    
}


