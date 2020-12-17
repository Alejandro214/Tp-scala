package o3.Analisis

import o3.Analisis.VariableDeclaradaNoUsadaMatcher.recursionBinaria
import o3.Analisis.VariableUsadaNoDeclaradaMatcher.declaredVariables
import o3.Mensajes.{LError, LMessage, LWarning}
import o3.Operaciones._
import o3.Valor.{LFalse, LTrue, Numero, ValorBooleano, ValorNumerico}
import o3.Variable.{Asignar, DeclararVariable, Variable}

trait TypeOfMessage
case class MyWarning() extends TypeOfMessage
case class MyError() extends TypeOfMessage

trait Matcher

trait SingleMatcher extends Matcher {
  def aplicaRegla(arg: Instruccion) : Boolean = {
    unapply(arg)
  }
  def unapply(arg: Instruccion): Boolean
}

trait GlobalMatcher extends Matcher {
  def aplicaRegla(arg : Array[Instruccion]) : Boolean

  def recorrerYAnalizar(instruccion: Instruccion): Unit

  def recursionBinaria(x : Instruccion, y : Instruccion): Unit = {
    recorrerYAnalizar(x)
    recorrerYAnalizar(y)
  }
}

case object VariableDeclaradaNoUsadaMatcher extends GlobalMatcher {

  var declaredVariables : Map[String, String] = Map()

  override def aplicaRegla(arg: Array[Instruccion]): Boolean = {

    arg.foreach( instruccion => {
      recorrerYAnalizar(instruccion)
    })

    val result = declaredVariables.exists( elem => {
      "Declarada".equals(elem._2)
    })

    declaredVariables = Map()
    result
  }

  override def recorrerYAnalizar(instruccion: Instruccion): Unit = {
    instruccion match {
      case DeclararVariable(name_variable, valor) =>
        if (!declaredVariables.contains(name_variable)) {
          declaredVariables += (name_variable -> "Declarada")
        }
        recorrerYAnalizar(valor)
      case Asignar(name_variable, valor) =>
        declaredVariables += (name_variable.name_variable -> "Asignada")
        recorrerYAnalizar(valor)
      case Variable(name_variable) =>
        declaredVariables += (name_variable -> "Usada")
      case instruccion: OperacionBooleano =>
        recursionBinaria(instruccion.v_1, instruccion.v_2)
      case instruccion: OperacionBinaria =>
        recursionBinaria(instruccion.valor_1, instruccion.valor_2)
      case _ =>
    }
  }

}

case object VariableUsadaAntesDeSerDeclaradaMatcher extends GlobalMatcher {

  var declaredVariables : Map[String, String] = Map()

  override def aplicaRegla(arg: Array[Instruccion]): Boolean = {

    arg.foreach( instruccion => {
      recorrerYAnalizar(instruccion)
    })

    val result = declaredVariables.exists( elem => {
      "Declarada".equals(elem._2)
    })

    declaredVariables = Map()
    result
  }

  override def recorrerYAnalizar(instruccion: Instruccion): Unit = {
    instruccion match {
      case DeclararVariable(name_variable, valor) =>
        if (declaredVariables.getOrElse(name_variable, "").equals("Usada") ) {
          declaredVariables += (name_variable -> "Declarada")
        }
        recorrerYAnalizar(valor)
      case Asignar(name_variable, valor) =>
        if (!declaredVariables.contains(name_variable.name_variable)) {
          declaredVariables += (name_variable.name_variable -> "Usada")
        }
        recorrerYAnalizar(valor)
      case Variable(name_variable) =>
        if (!declaredVariables.contains(name_variable)) {
          declaredVariables += (name_variable -> "Usada")
        }
      case instruccion: OperacionBooleano =>
        recursionBinaria(instruccion.v_1, instruccion.v_2)
      case instruccion: OperacionBinaria =>
        recursionBinaria(instruccion.valor_1, instruccion.valor_2)
      case _ =>
    }
  }

}

case object VariableYaDeclaradaMatcher extends GlobalMatcher {

  var declaredVariables : Map[String, String] = Map()

  override def aplicaRegla(arg: Array[Instruccion]): Boolean = {

    arg.foreach( instruccion => {
      recorrerYAnalizar(instruccion)
    })

    val result = declaredVariables.exists( elem => {
      "YaDeclarada".equals(elem._2)
    })

    declaredVariables = Map()
    result
  }

  override def recorrerYAnalizar(instruccion: Instruccion): Unit = {
    instruccion match {
      case DeclararVariable(name_variable, valor) =>
        if (! declaredVariables.contains(name_variable)) {
          declaredVariables += (name_variable -> "Declarada")
        } else {
          declaredVariables += (name_variable -> "YaDeclarada")
        }
        recorrerYAnalizar(valor)
      case Asignar(_, valor) =>
        recorrerYAnalizar(valor)
      case instruccion: OperacionBooleano =>
        recursionBinaria(instruccion.v_1, instruccion.v_2)
      case instruccion: OperacionBinaria =>
        recursionBinaria(instruccion.valor_1, instruccion.valor_2)
      case _ =>
    }
  }

}

case object VariableUsadaNoDeclaradaMatcher extends GlobalMatcher {

  var declaredVariables : Map[String, String] = Map()

  override def aplicaRegla(arg: Array[Instruccion]): Boolean = {

    arg.foreach( instruccion => {
      recorrerYAnalizar(instruccion)
    })

    val result = declaredVariables.exists( elem => {
      "Usada".equals(elem._2)
    })

    declaredVariables = Map()
    result
  }

  override def recorrerYAnalizar(instruccion: Instruccion): Unit = {
    instruccion match {
      case DeclararVariable(name_variable, valor) =>
        declaredVariables += (name_variable -> "Declarada")
        recorrerYAnalizar(valor)
      case Asignar(name_variable, valor) =>
        if (! declaredVariables.getOrElse(name_variable.name_variable, "").equals("Declarada")) {
          declaredVariables += (name_variable.name_variable -> "Usada")
        }
        recorrerYAnalizar(valor)
      case Variable(name_variable) =>
        if (! declaredVariables.getOrElse(name_variable, "").equals("Declarada")) {
          declaredVariables += (name_variable -> "Usada")
        }
      case instruccion: OperacionBooleano =>
        recursionBinaria(instruccion.v_1, instruccion.v_2)
      case instruccion: OperacionBinaria =>
        recursionBinaria(instruccion.valor_1, instruccion.valor_2)
      case _ =>
    }
  }

}

case object SumaRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Suma(_, Numero(0)) => true
      case Suma(Numero(0), _) => true
      case _ => false
    }
  }

}

case object RestaRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Resta(_, Numero(0)) => true
      case _ => false
    }
  }

}

case object MultiplicacionPorCeroMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Multiplicacion(_, Numero(0)) => true
      case Multiplicacion(Numero(0), _) => true
      case _ => false
    }
  }

}

case object MultiplicacionPorUnoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Multiplicacion(_, Numero(1)) => true
      case Multiplicacion(Numero(1), _) => true
      case _ => false
    }
  }

}

case object DivisionPorCeroMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Division(_, Numero(0)) => true
      case _ => false
    }
  }

}

case object DivisionPorUnoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion) : Boolean = {
    arg match {
      case Division(_, Numero(1)) => true
      case _ => false
    }
  }

}

case object MenorRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Menor(Numero(n), Numero(m)) => true
      case _ => false
    }
  }

}

case object MenorInvalidoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Menor(v : ValorBooleano, y: ValorNumerico) => true
      case Menor(v : ValorNumerico, y: ValorBooleano) => true
      case _ => false
    }
  }

}

case object MenorIgualRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case MenorIgual(Numero(n), Numero(m)) => true
      case _ => false
    }
  }

}

case object MenorIgualInvalidoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case MenorIgual(v : ValorBooleano, y: ValorNumerico) => true
      case MenorIgual(v : ValorNumerico, y: ValorBooleano) => true
      case _ => false
    }
  }

}

case object MayorRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Mayor(Numero(n), Numero(m)) => true
      case _ => false
    }
  }

}

case object MayorInvalidoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Mayor(v : ValorBooleano, y: ValorNumerico) => true
      case Mayor(v : ValorNumerico, y: ValorBooleano) => true
      case _ => false
    }
  }

}

case object MayorIgualRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case MayorIgual(Numero(n), Numero(m)) => true
      case _ => false
    }
  }

}

case object MayorIgualInvalidoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case MayorIgual(v : ValorBooleano, y: ValorNumerico) => true
      case MayorIgual(v : ValorNumerico, y: ValorBooleano) => true
      case _ => false
    }
  }

}

case object IgualRedundanteMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Igual(Numero(n), Numero(m)) => true
      case Igual(LTrue(), LTrue()) => true
      case Igual(LTrue(), LFalse()) => true
      case Igual(LFalse(), LTrue()) => true
      case Igual(LFalse(), LFalse()) => true
      case _ => false
    }
  }

}

case object IgualInvalidoMatcher extends SingleMatcher {

  override def unapply(arg: Instruccion): Boolean = {
    arg match {
      case Igual(v : ValorBooleano, y: ValorNumerico) => true
      case Igual(v : ValorNumerico, y: ValorBooleano) => true
      case _ => false
    }
  }

}


abstract class Rule(var message : String, var matcher: Matcher, var typeOfMessage: TypeOfMessage) {
  def aplicaRegla(program: Array[Instruccion]): Boolean
  def obtenerMensaje(instruccion: Instruccion) : LMessage
}

class LineRule (message : String, matcher : SingleMatcher, typeOfM : TypeOfMessage) extends Rule(message, matcher, typeOfM) {

  override def aplicaRegla(program: Array[Instruccion]): Boolean = {
    matcher.aplicaRegla(program(0))
  }

  override def obtenerMensaje(instruccion: Instruccion) : LMessage = {
    typeOfM match {
      case MyError() => new LError(message, instruccion)
      case MyWarning() => new LWarning(message, instruccion)
      case _ => throw new Exception("Tipo de mensaje no valido")
    }
  }

}

class GlobalRule (message : String, matcher : GlobalMatcher, typeOfM : TypeOfMessage) extends Rule(message, matcher, typeOfM) {

  override def aplicaRegla(program: Array[Instruccion]): Boolean = {
    matcher.aplicaRegla(program)
  }

  override def obtenerMensaje(instruccion: Instruccion) : LMessage = {
    typeOfM match {
      case MyError() => new LError(message, null)
      case MyWarning() => new LWarning(message, null)
      case _ => throw new Exception("Tipo de mensaje no valido")
    }
  }

}
