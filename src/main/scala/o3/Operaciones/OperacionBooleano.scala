package o3.Operaciones

import o3.Valor.{Valor, ValorBooleano}

trait OperacionBooleano{
  var v_1:Valor
  var v_2:Valor
}

case class Menor(valor_1: Valor, valor_2: Valor) extends OperacionBooleano with ValorBooleano with Instruccion{
  override var v_1: Valor = valor_1
  override var v_2: Valor = valor_2
}

case class Mayor(valor_1: Valor,valor_2: Valor) extends OperacionBooleano with ValorBooleano with Instruccion {
  override var v_1: Valor = valor_1
  override var v_2: Valor = valor_2
}

case class Igual(valor_1:Valor,valor_2:Valor) extends OperacionBooleano with ValorBooleano with Instruccion {
  override var v_1: Valor = valor_1
  override var v_2: Valor = valor_2
}

case class MenorIgual(valor_1: Valor,valor_2: Valor) extends OperacionBooleano with ValorBooleano with Instruccion {
  override var v_1: Valor = valor_1
  override var v_2: Valor = valor_2
}

case class MayorIgual(valor_1: Valor,valor_2: Valor) extends OperacionBooleano with ValorBooleano with Instruccion {
  override var v_1: Valor = valor_1
  override var v_2: Valor = valor_2
}
