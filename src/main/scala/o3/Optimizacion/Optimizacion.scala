package o3.Optimizacion

import o3.IF.If
import o3.Valor.{LFalse, LTrue, Numero, Valor, ValorBooleano, ValorNumerico, ValorVariable}
import o3.Operaciones.{Division, Igual, Instruccion, Mayor, MayorIgual, Menor, MenorIgual, Multiplicacion, OperacionBinaria, OperacionBooleano, Resta, Suma}
import o3.Variable.{Asignar, DeclararVariable, Variable}

class Optimizacion {

  var variables_usadas: Array[String] = Array();

  def optimizar_programa(programa: Array[Instruccion]): Array[Instruccion] = {
    var array_de_instrucciones: Array[Instruccion] = Array();
    registrar_variables_de_programa(programa) //Registro las variables que se usan el programa
    programa.foreach((currentInstruction: Instruccion) => {
      array_de_instrucciones = array_de_instrucciones.concat(Array(optimizar_instruccion(currentInstruction)))
    })
    variables_usadas = Array()
    array_de_instrucciones.filter(map => map != null)

  }

  private def optimizar_instruccion(una_instruccion: Instruccion): Valor = una_instruccion match {
    case una_instruccion: ValorVariable => optimizar_Variable(una_instruccion)
    case una_instruccion: ValorNumerico => optimizar_Operacion_Aritmetica(una_instruccion)
    case una_instruccion: ValorBooleano => optimizar_Operacion_Booleana(una_instruccion)
    case una_instruccion: Asignar => optimizar_operacion_Asignar(una_instruccion)
  }

  //Dada una operacion Aritmetica, en el caso que sea un numero, retorno el numero
  //En el caso que sea una Operacion Binaria, optimizo a sus hijos y luego a la Operacion Binaria
  private def optimizar_Operacion_Aritmetica(operacion_aritmetica: Valor): Valor = operacion_aritmetica match {
    case Numero(n) => Numero(n)
    //Caso recursivo
    case operacionBinaria: OperacionBinaria => optimizar_casos_de_operacion_Binaria(optimizar_hijos_de_operacion_Binaria(operacionBinaria))

  }

  //Dada una operacion Binaria, optimizo a sus dos hijos y retorno la operacionBinaria con sus
  //Hijos optimizados
  private def optimizar_hijos_de_operacion_Binaria(operacionBinaria: OperacionBinaria): Valor = {
    var new_value_v_1 = optimizar_instruccion(operacionBinaria.valor_1)
    var new_value_v_2 = optimizar_instruccion(operacionBinaria.valor_2)
    new_operacion_Binaria(operacionBinaria, new_value_v_1, new_value_v_2)

  }

  def optimizar_casos_de_operacion_Binaria(operacionAritmetica: Valor) = operacionAritmetica match {
    //Casos de Suma
    case Suma(valor_1, Numero(0)) => optimizar_instruccion(valor_1)
    case Suma(Numero(0), valor_2) => optimizar_instruccion(valor_2)

    //Casos de Resta
    case Resta(valor_1, Numero(0)) => optimizar_instruccion(valor_1)

    //Casos de Multiplicacion
    case Multiplicacion(valor_1, Numero(1)) => optimizar_instruccion(valor_1)
    case Multiplicacion(Numero(1), valor_2) => optimizar_instruccion(valor_2)
    //Caso de division
    case Division(valor_1, Numero(1)) => optimizar_instruccion(valor_1)

    case operacion_aritmetica => operacion_aritmetica
  }

  private def new_operacion_Binaria(operacionBinaria: OperacionBinaria, v_1: Valor, v_2: Valor) = operacionBinaria match {
    case operacionBinaria: Division => Division(v_1, v_2)
    case operacionBinaria: Multiplicacion => Multiplicacion(v_1, v_2)
    case operacionBinaria: Resta => Resta(v_1, v_2)
    case operacionBinaria: Suma => Suma(v_1, v_2)

  }

  //Al igual que la operacion Binaraia, si la operacion Booleana es LTRUE() o LFalse(), retorno alguno de ellos
  //Si es una Operacion Booleana, optimizo a sus hijos y luego a la Operacion Booleana
  private def optimizar_Operacion_Booleana(operacion_Booleana: ValorBooleano): Valor = operacion_Booleana match {
    //Casos base
    case LTrue() => LTrue()
    case LFalse() => LFalse()
    //Casos recursivos
    case operacionBooleano: OperacionBooleano => optimizar_casos_de_operacion_booleana(optimizar_hijos_de_operacion_Booleana(operacionBooleano))
  }

  private def optimizar_hijos_de_operacion_Booleana(operacionBooleano: OperacionBooleano): Valor = {
    var new_value_v_1 = optimizar_instruccion(operacionBooleano.v_1)
    var new_value_v_2 = optimizar_instruccion(operacionBooleano.v_2)
    new_operacion_Booleana(operacionBooleano, new_value_v_1, new_value_v_2)
  }

  private def new_operacion_Booleana(operacionBooleano: OperacionBooleano, v_1: Valor, v_2: Valor): ValorBooleano = operacionBooleano match {
    case operacionBooleano: Menor => Menor(v_1, v_2)
    case operacionBooleano: Mayor => Mayor(v_1, v_2)
    case operacionBooleano: Igual => Igual(v_1, v_2)
    case operacionBooleano: MayorIgual => MayorIgual(v_1, v_2)
    case operacionBooleano: MenorIgual => MenorIgual(v_1, v_2)
  }


  def optimizar_casos_de_operacion_booleana(operacion_Booleana: Valor) = operacion_Booleana match {
    //Casos de Menor
    case Menor(Numero(numero_1), Numero(numero_2)) => boolean_de_operacion(numero_1 < numero_2)

    //Casos de Mayor
    case Mayor(Numero(numero_1), Numero(numero_2)) => boolean_de_operacion(numero_1 > numero_2)

    //Caso de Igual
    case Igual(Numero(numero_1), Numero(numero_2)) => boolean_de_operacion(numero_1 == numero_2)
    case Igual(valor_1: ValorBooleano, valor_2: ValorBooleano) => boolean_de_operacion(valor_1 == valor_2)
    //Caso de MenorIgual
    case MenorIgual(Numero(numero_1), Numero(numero_2)) => boolean_de_operacion(numero_1 <= numero_2)
    case MayorIgual(Numero(numero_1), Numero(numero_2)) => boolean_de_operacion(numero_1 >= numero_2)
    case operacion_Booleana => operacion_Booleana

  }

  private def optimizar_Variable(variable: ValorVariable): Valor = variable match {
    case declararVariable: DeclararVariable => optimizar_declaracion_Variable_si_es_usada(declararVariable)
    case varible => varible
  }

  private def optimizar_operacion_Asignar(asignar: Asignar): Valor = asignar match {
    case Asignar(variable, valor) => Asignar(variable, optimizar_instruccion(valor))
  }

  private def registrar_variables_de_programa(programa: Array[Instruccion]): Unit = {
    programa.foreach((concurrentInstruction: Instruccion) => {
      registrar_variable_que_es_usada(concurrentInstruction)
    })
  }

  //
  private def registrar_variable_que_es_usada(una_instruccion: Instruccion): Unit = una_instruccion match {
    case Variable(name_variable) => add_name_Variable_usada(name_variable)
    case Asignar(variable, valor) => add_name_Variable_usada(variable.name_variable); registrar_variable_que_es_usada(valor)
    case una_instruccion: OperacionBinaria => registrar_variable_que_es_usada(una_instruccion.valor_1); registrar_variable_que_es_usada(una_instruccion.valor_2)
    case una_instruccion: OperacionBooleano => registrar_variable_que_es_usada(una_instruccion.v_1); registrar_variable_que_es_usada(una_instruccion.v_2)
    case If(condicion, programa_1, programa_2) => registrar_variable_que_es_usada(condicion); registrar_variables_de_programa(programa_1); registrar_variables_de_programa(programa_2)
    case _ => None
  }


  private def add_name_Variable_usada(name_variable: String): Unit = {
    variables_usadas = variables_usadas :+ name_variable
  }

  //Si la Declaracion de la variable es usada, la optimizo y la retorno
  //En el caso que no sea usada, retorno Null
  private def optimizar_declaracion_Variable_si_es_usada(declararVariable: DeclararVariable): Valor ={
    if(variables_usadas.contains(declararVariable.name_variable)){
        optimizar_declaracion_variable(declararVariable)
     }
    else {
       null
     }
  }
  //Optimizo al hijo derecho de la declaracion de variable
  private def optimizar_declaracion_variable(declararVariable: DeclararVariable) = declararVariable match {
    case DeclararVariable(name_variable, valor) => DeclararVariable(name_variable, optimizar_instruccion(valor))
  }

  private def boolean_de_operacion(boolean: Boolean):ValorBooleano ={
    if (boolean)  LTrue() else  LFalse()
  }






  private def optimizar_IF(un_if:If):Any = un_if match {
    case If(LTrue(),rama_positiva,_)                           => optimizar_programa(rama_positiva)
    case If(LFalse(),_,rama_negativa)                          => optimizar_programa(rama_negativa)
    case If(condicion,Array(LTrue()),Array(LFalse()))          => optimizar_instruccion(condicion)
    case If(condicion,Array(LFalse()),Array(LTrue()))          => optimizar_instruccion(condicion)
    case If(condicion,rama_positiva,rama_negativa)             => If(optimizar_instruccion(condicion),optimizar_programa(rama_positiva),optimizar_programa(rama_negativa))
  }


}
