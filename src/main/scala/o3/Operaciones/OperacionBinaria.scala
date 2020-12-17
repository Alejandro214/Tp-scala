package o3.Operaciones


import o3.Valor.{Valor, ValorNumerico};

trait Instruccion

trait OperacionBinaria extends Valor {
 var valor_1:Valor
 var valor_2:Valor


}



case class Suma(numerio_1: Valor, numerio_2: Valor) extends OperacionBinaria with   ValorNumerico with Instruccion {
 override var valor_1: Valor = numerio_1
 override var valor_2: Valor = numerio_2

}

case class Resta(numerio_1: Valor,numerio_2: Valor) extends  OperacionBinaria with ValorNumerico with Instruccion {
 override var valor_1: Valor = numerio_1
 override var valor_2: Valor = numerio_2
}

case class Division(numerio_1: Valor,numerio_2: Valor) extends  OperacionBinaria with ValorNumerico with Instruccion {
 override var valor_1: Valor = numerio_1
 override var valor_2: Valor = numerio_2
}

case class Multiplicacion(numerio_1: Valor,numerio_2: Valor) extends  OperacionBinaria with ValorNumerico with Instruccion {
 override var valor_1: Valor = numerio_1
 override var valor_2: Valor = numerio_2
}
