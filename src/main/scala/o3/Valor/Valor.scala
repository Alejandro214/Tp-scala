package o3.Valor

import o3.Operaciones.Instruccion


trait Valor extends Instruccion


trait ValorNumerico extends Valor

trait ValorVariable extends Valor


trait ValorBooleano extends Valor


//Numero
case class Numero(numero:Int) extends ValorNumerico

//Booleanos True o False
case class LTrue() extends ValorBooleano

case class LFalse() extends ValorBooleano
