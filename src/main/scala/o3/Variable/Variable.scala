package o3.Variable

import o3.Operaciones.Instruccion
import o3.Valor.{Valor, ValorVariable}


case class Variable(name_variable:String) extends ValorVariable with Instruccion

case class AsignarVariable(variable : Variable, valor : Valor) extends Instruccion

case class DeclararVariable(name_variable:String,valor:Valor) extends ValorVariable with Instruccion


case class Asignar(variable:Variable,valor:Valor) extends  Valor with  Instruccion
