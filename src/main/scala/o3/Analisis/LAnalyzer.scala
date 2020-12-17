package o3.Analisis

import o3.Mensajes.LMessage
import o3.Operaciones.{Instruccion, OperacionBinaria, OperacionBooleano}
import o3.Valor.Valor
import o3.Variable.{AsignarVariable, DeclararVariable}

sealed trait TypeOfVar

case class VarDeclared() extends TypeOfVar
case class VarDeclaredAndUsed() extends TypeOfVar
case class VarUsedAndDeclared() extends TypeOfVar
case class VarUsed() extends TypeOfVar

class LAnalyzer {

  def analyze(instr : Instruccion, rules : Array[Rule]): Array[LMessage] = {

    var messages : Array[LMessage] = Array()

    messages = analize_single_instruction_rules(instr, rules, messages)

    instr match {
      case boolOp: OperacionBooleano =>
        messages = messages.concat(nuevo_totalRecursion(boolOp.v_1, boolOp.v_2, rules))
      case aritOp: OperacionBinaria =>
        messages = messages.concat(nuevo_totalRecursion(aritOp.valor_1, aritOp.valor_2, rules))
      case DeclararVariable(x,y) =>
        messages = messages.concat(analyze(y, rules))
      case AsignarVariable(x,y) =>
        messages = messages.concat(analyze(y, rules))
      case _ =>
    }

    messages
  }

  def analyze(program : Array[Instruccion], rules : Array[Rule]): Array[LMessage] = {
    var currentMessages : Array[LMessage] = Array()

    currentMessages = analize_global_rules(program, rules, currentMessages)

    program.foreach( (currentInstruction : Instruccion) => {
      val instructionAnalyzed = analyze(currentInstruction, rules)
      currentMessages = currentMessages.concat(instructionAnalyzed)
    })
    currentMessages
  }

  private def nuevo_totalRecursion(x: Valor, y: Valor, rules: Array[Rule]) : Array[LMessage] = {
    val otherEval1 = analyze(x, rules)
    val otherEval2 = analyze(y, rules)
    otherEval1.concat(otherEval2)
  }

  private def analize_global_rules(program: Array[Instruccion], rules : Array[Rule], messages : Array[LMessage]) : Array[LMessage] = {
    var currentMessages = messages
    rules.foreach(rule => {
      rule.matcher match {
        case _: GlobalMatcher =>
          if (rule.aplicaRegla(program)) {
            currentMessages = currentMessages.concat(Array(rule.obtenerMensaje(program(0))))
          }
        case _ =>
      }
    })
    currentMessages
  }

  private def analize_single_instruction_rules(instr: Instruccion, rules: Array[Rule], messages: Array[LMessage]) : Array[LMessage] = {
    var currentMessages = messages
    rules.foreach(rule => {
      rule.matcher match {
        case _: SingleMatcher =>
          if (rule.aplicaRegla(Array(instr))) {
            currentMessages = currentMessages.concat(Array(rule.obtenerMensaje(instr)))
          }
        case _ =>
      }
    })
    currentMessages
  }
}

