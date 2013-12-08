package com.ubikuitous.experimentalvm.vm

trait Reference 

trait CodePointer 

sealed abstract class Value 
object Value {
  case class INTEGER(value : BigInt) extends Value
  case class CLOSURE(cp : CodePointer, gp : Reference) extends Value
  case class FUNCTION(cp : CodePointer, ap : Reference, gp : Reference) extends Value
  case class VECTOR(elements: Array[Reference]) extends Value
  case class SYMBOL(name : String) extends Value
}

trait Heap {
  def alloc(value : Value) : Reference
  def resolve(reference : Reference) : Value
  def update(reference : Reference, value : Value)
}

trait Stack {
  var SP : Long
  var FP : Long 
  
  def get(sp : Long) : Reference
  def set(sp : Long, r : Reference)
    
  def push(r : Reference) { 
    SP = SP + 1
    set(SP, r)
  }
  
  def pop() : Reference = {
    val r = get(SP)
    SP = SP - 1
    r
  } 
}

trait ProgramStore {

  // set the CP to cp
  def goto(cp : CodePointer)
  
  // set the CP to (cp + i)
  def goto(cp : CodePointer, i : Long)
  
  // reads the instruction at CP and increments CP
  def read() : Instruction
  
  // the current CP
  def CP : CodePointer

}

sealed abstract class Instruction 
object Instruction {
  case object HALT extends Instruction
  case object CRASH extends Instruction
  case class LOADI(i : Value.INTEGER) extends Instruction
  case class LOADS(s : Value.SYMBOL) extends Instruction
  case object NEG extends Instruction
  case object ADD extends Instruction 
  case object SUB extends Instruction
  case object MUL extends Instruction
  case object DIV extends Instruction
  case object MOD extends Instruction
  case object AND extends Instruction
  case object OR extends Instruction
  case object NOT extends Instruction
  case object EQ extends Instruction
  case object NEQ extends Instruction
  case object LE extends Instruction
  case object LEQ extends Instruction
  case object GR extends Instruction
  case object GEQ extends Instruction
  case class JUMP(cp : CodePointer) extends Instruction
  case class JUMPZ(cp : CodePointer) extends Instruction
  case class JUMPI(cp : CodePointer) extends Instruction
}

trait VM {
  val stack : Stack
  val heap : Heap
  val programStore : ProgramStore
  var GP : Reference  
  
  def resolve(delta : Long) : Value = 
    heap.resolve(stack.get(stack.SP - delta))
    
  def pop() : Value = {
    heap.resolve(stack.pop())
  }
  
  def push(value : Value) = {
    stack.push(heap.alloc(value))
  }
}

trait RunVM extends VM {
  
  def run() {
    var instr : Instruction = null
    do {
      instr = programStore.read()
    } while (execute(instr))
  }

  private def crash() {
    throw new RuntimeException("VM crashed")
  }

  private def execute(instruction : Instruction) : Boolean = {
    import Instruction._
    instruction match {
      case HALT => return false
      case CRASH => crash()
      case LOADI(i) => instrLOADI(i)
      case LOADS(s) => instrLOADS(s)
      case NEG => instrNEG()
      case ADD => instrADD()
      case SUB => instrSUB()
      case MUL => instrMUL()
      case DIV => instrDIV()
      case MOD => instrMOD()
      //case AND => instrAND()
      //case OR => instrOR()
      case JUMP(cp) => instrJUMP(cp)
      case JUMPZ(cp) => instrJUMPZ(cp)
      case JUMPI(cp) => instrJUMPI(cp)
    }
    return true
  }
  
  import Value._
  
  private def instrNEG() {
    pop() match {
      case INTEGER(x) => push(new INTEGER(-x))
      case _ => crash()     
    }
  }
  
  private def instrADD() {
    (pop(), pop()) match {
      case (INTEGER(x), INTEGER(y)) => push(new INTEGER(x + y))
      case _ => crash()
    }  
  }
  
  private def instrSUB() {
    (pop(), pop()) match {
      case (INTEGER(x), INTEGER(y)) => push(new INTEGER(x - y))
      case _ => crash()
    }      
  }

  private def instrMUL() {
    (pop(), pop()) match {
      case (INTEGER(x), INTEGER(y)) => push(new INTEGER(x * y))
      case _ => crash()
    }      
  }
  
  private def instrDIV() {
    (pop(), pop()) match {
      case (INTEGER(x), INTEGER(y)) => 
        if (y == 0) crash() else push(new INTEGER(x / y))
      case _ => crash()
    }      
  }

  private def instrMOD() {
    (pop(), pop()) match {
      case (INTEGER(x), INTEGER(y)) => 
        if (y == 0) crash() else push(new INTEGER(x % y))
      case _ => crash()
    }      
  }
  
  private def instrLOADI(i : INTEGER) {
    push(i)
  }

  private def instrLOADS(s : SYMBOL) {
    push(s)
  }
  
  private def instrJUMP(cp : CodePointer) {
    programStore.goto(cp)
  }
  
  private def instrJUMPZ(cp : CodePointer) {
    pop() match {
      case INTEGER(x) => 
        if (x == 0) programStore.goto(cp)
      case _ => crash()
    }
  }
  
  private def instrJUMPI(cp : CodePointer) {
    pop() match {
      case INTEGER(x) =>
        if (x.isValidLong) programStore.goto(cp, x.toLong)
        else crash()
      case _ => crash()
    }
  }
  
}




