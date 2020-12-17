package o3.IF

import o3.Operaciones.Instruccion
import o3.Valor.{Valor, ValorBooleano}
import o3.Variable.Variable

case class If(valorBooleano: Valor,programa_1: Array[Instruccion],programa_2: Array[Instruccion]) extends Valor with Instruccion


