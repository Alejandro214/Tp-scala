package o3
import o3.Ejecutor.Ejecutor
import o3.Expecion.{UndefinedVariable,NotCanRedefinedType,DuplicateVariable,OperationNonsense}
import o3.Operaciones.{Division, Igual, Instruccion, Mayor, MayorIgual, Menor, MenorIgual, Multiplicacion, Resta, Suma}
import o3.Valor.{LFalse, LTrue, Numero}
import o3.Variable.{Asignar, DeclararVariable, Variable}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ASTEjecutadorSpec extends AnyFunSpec with Matchers {

  var ejecutor:Ejecutor = new Ejecutor()

  it("Ejecuto la suma"){
    val numero_1 = Numero(1);
    val numero_2 = Numero(2);
    val suma     = Suma(numero_1,numero_2)
    val programa:Array[Instruccion] = Array(suma)
    ejecutor.ejecutar_programa(programa) should equal(3)
  }
  it("Ejecucion de la resta"){
    val numero_1 = Numero(1);
    val numero_2 = Numero(1);
    val resta    = Resta(numero_1,numero_2);
    val programa:Array[Instruccion] = Array(resta)
    ejecutor.ejecutar_programa(programa) should equal(0)
  }

  it("Ejecucion de la Multiplicacion"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val multiplicacion = Multiplicacion(numero_1,numero_2)
    val programa:Array[Instruccion] = Array(multiplicacion)
    ejecutor.ejecutar_programa(programa) should equal(10)
  }

  it("Ejecucion de la division"){
    val numero_1 = Numero(10);
    val numero_2 = Numero(2);
    val division = Division(numero_1,numero_2);
    val programa:Array[Instruccion] = Array(division)
    ejecutor.ejecutar_programa(programa) should equal(5)
  }

  it("Ejecucion del menor"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val menor    = Menor(numero_1,numero_2)
    val programa:Array[Instruccion] = Array(menor)
    ejecutor.ejecutar_programa(programa) should equal(true)
  }

  it("Ejecucion del mayor"){
    val numero_1 = Numero(10);
    val numero_2 = Numero(5);
    val mayor    = Mayor(numero_1,numero_2)
    val programa:Array[Instruccion] = Array(mayor)
    ejecutor.ejecutar_programa(programa) should equal(true)
  }

  it("Ejecucion del igual"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val igual    = Igual(numero_1,numero_2);
    val programa:Array[Instruccion] = Array(igual)
    ejecutor.ejecutar_programa(programa) should equal(false)
  }

  it("Ejecucion del MayorIgual"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val mayor_Igual = MayorIgual(numero_1,numero_2);
    val programa:Array[Instruccion] = Array(mayor_Igual)
    ejecutor.ejecutar_programa(programa) should equal(false)
  }

  it("Ejecucion del MenorIgual"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val menor_Igual = MenorIgual(numero_1,numero_2);
    val programa:Array[Instruccion] = Array(menor_Igual)
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de un suma con otros AST"){
    val numero_1 = Numero(2);
    val numero_2 = Numero(5);
    val suma_1     = Suma(numero_1,numero_2)
    val multiplicacion = Multiplicacion(numero_1,numero_2)
    val suma_2     = Suma(suma_1,multiplicacion)
    val programa:Array[Instruccion] = Array(suma_2)
    ejecutor.ejecutar_programa(programa) should equal(17)
  }


  it("Ejecucion de operacion booleana"){
    val numero_1    = Numero(10);
    val numero_2    = Numero(2);
    val division    = Division(numero_1,numero_2);
    val resta       = Resta(numero_1,numero_2);
    val menor       = Menor(resta,division) ;
    val programa:Array[Instruccion] = Array(menor)
    ejecutor.ejecutar_programa(programa) should equal(false)
  }

  it("Ejecucion de un Numero"){
    val programa:Array[Instruccion] = Array(Numero(1))
    ejecutor.ejecutar_programa(programa) should equal(1)

  }
  it("Ejecucion de Boolean LTrue"){
    val programa:Array[Instruccion] = Array(LTrue())
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de Boolean LFalse"){
    val programa:Array[Instruccion] = Array(LFalse())
    ejecutor.ejecutar_programa(programa) should equal(false)
  }

  it("Ejecucion de la creacion y lectura de una variable booleana"){
    val programa:Array[Instruccion] = Array(DeclararVariable("un_bool",LTrue()),Variable("un_bool"))
    ejecutor.ejecutar_programa(programa) should equal(true)

  }
  it("Ejecucion de la creacion y lectura de una variable numerica"){
    val programa:Array[Instruccion] = Array(DeclararVariable("un_numero",Numero(2)),Variable("un_numero"))
    ejecutor.ejecutar_programa(programa) should equal(2)
  }
  it("Ejecucion de la creacion de una variable a partir de una suma"){
    val programa:Array[Instruccion] = Array(DeclararVariable("resul_suma",Suma(Numero(1),Numero(2))),Variable("resul_suma"))
    ejecutor.ejecutar_programa(programa) should equal(3)
  }
  it("Ejecucion de la creacion de una variable a partir de una resta"){
    val programa:Array[Instruccion] = Array(DeclararVariable("resul_resta",Resta(Numero(3),Numero(2))),Variable("resul_resta"))
    ejecutor.ejecutar_programa(programa) should equal(1)
  }
  it("Ejecucion de la creacion de una variable a partir de una multiplicacion"){
    val programa:Array[Instruccion] = Array(DeclararVariable("resul_mul",Multiplicacion(Numero(3),Numero(2))),Variable("resul_mul"))
    ejecutor.ejecutar_programa(programa) should equal(6)
  }
  it("Ejecucion de la creacion de una variable a partir de una division"){
    val programa:Array[Instruccion] = Array(DeclararVariable("resul_div",Division(Numero(2),Numero(2))),Variable("resul_div"))
    ejecutor.ejecutar_programa(programa) should equal(1)
  }
  it("Ejecucion de la creacion de una variable a partir de una operacion de menor"){
    val programa:Array[Instruccion] = Array(DeclararVariable("result_menor",Menor(Numero(1),Numero(5))),Variable("result_menor"))
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de la creacion de una variable a partir de una operacion de comparacion"){
    val programa:Array[Instruccion] = Array(DeclararVariable("result_eq",Igual(Numero(1),Numero(5))),Variable("result_eq"))
    ejecutor.ejecutar_programa(programa) should equal(false)
  }
  it("Ejecucion de la asignacion de un nuevo numero a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("un_num",Numero(1)),Asignar(Variable("un_num"),Numero(10)),Variable("un_num"))
    ejecutor.ejecutar_programa(programa) should equal(10)
  }
  it("Ejecucion de la asignacion de un nuevo booleano a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("un_num",LTrue()),Asignar(Variable("un_num"),LFalse()),Variable("un_num"))
    ejecutor.ejecutar_programa(programa) should equal(false)
  }
  it("Ejecucion de la asignacion de una suma a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("suma",Suma(Numero(1),Numero(8))),Asignar(Variable("suma"),Numero(2)),Variable("suma"))
    ejecutor.ejecutar_programa(programa) should equal(2)
  }
  it("Ejecucion de la asignacion de una resta a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("resta",Resta(Numero(10),Numero(8))),Asignar(Variable("resta"),Numero(5)),Variable("resta"))
    ejecutor.ejecutar_programa(programa) should equal(5)
  }
  it("Ejecucion de la asignacion de una multiplicacion a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("mul",Multiplicacion(Numero(1),Numero(8))),Asignar(Variable("mul"),Numero(20)),Variable("mul"))
    ejecutor.ejecutar_programa(programa) should equal(20)
  }
  it("Ejecucion de la asignacion de una division a una variable"){
    val programa:Array[Instruccion] = Array(DeclararVariable("div",Division(Suma(Numero(2),Numero(14)),Numero(8))),Asignar(Variable("div"),Numero(6)),Variable("div"))
    ejecutor.ejecutar_programa(programa) should equal(6)
  }
  it("Ejecucion de una lista de instrucciones"){
    val programa:Array[Instruccion] = Array(DeclararVariable("un_num",Numero(3)),DeclararVariable("otro_num",Numero(5)))
    ejecutor.ejecutar_programa(programa) should equal(5)
  }
  it("Ejecucion de la suma de una variable numerica con un numero"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),Suma(Variable("num"),Numero(5)))
    ejecutor.ejecutar_programa(programa) should equal(15)
  }
  it("Ejecucion de la resta de una variable numerica con un numero"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),Resta(Variable("num"),Numero(5)))
    ejecutor.ejecutar_programa(programa) should equal(5)
  }
  it("Ejecucion de la multiplicacion de una variable numerica con un numero"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),Multiplicacion(Numero(5),Variable("num")))
    ejecutor.ejecutar_programa(programa) should equal(50)
  }
  it("Ejecucion de la division de una variable numerica con un numero"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),Division(Variable("num"),Numero(5)))
    ejecutor.ejecutar_programa(programa) should equal(2)
  }
  it("Ejecucion de la operacion menor con variables numericas"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),DeclararVariable("num2",Numero(1)),Menor(Variable("num"),Variable("num2")))
    ejecutor.ejecutar_programa(programa) should equal(false)
  }
  it("Ejecucion de la operacion menorigual con variables numericas"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(10)),DeclararVariable("num2",Numero(10)),MenorIgual(Variable("num"),Variable("num2")))
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de la operacion igual con variables numericas"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num1",Numero(1)),DeclararVariable("num2",Numero(1)),Igual(Variable("num1"),Variable("num2")))
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de la operacion mayor con variables numericas"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num1",Numero(2)),DeclararVariable("num2",Numero(1)),Mayor(Variable("num1"),Variable("num2")))
    ejecutor.ejecutar_programa(programa) should equal(true)
  }
  it("Ejecucion de la operacion mayorigual con variables numericas"){
    val programa:Array[Instruccion] = Array(DeclararVariable("num1",Numero(2)),DeclararVariable("num2",Numero(2)),MayorIgual(Variable("num1"),Variable("num2")))
    ejecutor.ejecutar_programa(programa) should equal(true)
  }

  it("asigno un nuevo valor a una variable que no existe"){
    assertThrows[UndefinedVariable]{
      ejecutor.ejecutar_AST(Asignar(Variable("pepe"),Numero(10)))
    }
  }
  it("no puedo redefinir el tipo de valor de una variable"){
    assertThrows[NotCanRedefinedType]{
      val programa:Array[Instruccion] = Array(DeclararVariable("num",Numero(1)),Asignar(Variable("num"),LTrue()))
      ejecutor.ejecutar_programa(programa)
    }
  }
  it("busco una variable que nunca se declaro"){
    assertThrows[UndefinedVariable]{
      ejecutor.ejecutar_AST(Variable("algo"))
    }
  }
  it("declaro una variable que ya existe") {
    assertThrows[DuplicateVariable]{
    val programa: Array[Instruccion] = Array(DeclararVariable("num", Numero(1)), DeclararVariable("num", Numero(1)))
    ejecutor.ejecutar_programa(programa)
    }
  }
  it("ejecuto una suma con un numero y un booleano"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(Suma(LFalse(), Numero(1))))
    }
  }
  it("ejecuto una suma con una variable booleana y un numero"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Suma(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una resta con un numero y un booleano"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(Resta(LFalse(), Numero(1))))
    }
  }
  it("ejecuto una resta con una variable booleana y un numero"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Resta(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una multiplicacion con un numero y un booleano"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(Multiplicacion(LFalse(), Numero(1))))
    }
  }
  it("ejecuto una multiplicacion con una variable booleana y un numero"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Multiplicacion(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una division con un numero y un booleano"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(Division(LFalse(), Numero(1))))
    }
  }
  it("ejecuto una division con una variable booleana y un numero"){
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Division(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una operacion menor con una variable booleana y un numero") {
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Menor(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una operacion menor igual con una variable booleana y un numero") {
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), MenorIgual(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una operacion igual con una variable booleana y un numero") {
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Igual(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una operacion mayor con una variable booleana y un numero") {
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), Mayor(Numero(1), Variable("un_bol"))))
    }
  }
  it("ejecuto una operacion mayor igual con una variable booleana y un numero") {
    assertThrows[OperationNonsense] {
      ejecutor.ejecutar_programa(Array(DeclararVariable("un_bol", LFalse()), MayorIgual(Numero(1), Variable("un_bol"))))
    }
  }
}
