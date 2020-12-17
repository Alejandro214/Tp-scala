package o3


import o3.IF.If
import o3.Valor.{LFalse, LTrue, Numero}
import o3.Operaciones.{Division, Igual, Instruccion, Mayor, MayorIgual, Menor, MenorIgual, Multiplicacion, Resta, Suma}
import o3.Optimizacion.Optimizacion
import o3.Variable.{Asignar, DeclararVariable, Variable}
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ASTOptimizadorSpec extends AnyFunSpec with Matchers{
  var optimizacion:Optimizacion = new Optimizacion();

  //Casos bases
  it("No se optimiza mas Numero"){
    var numero_1 = Numero(1)
    var programa:Array[Instruccion]   = Array(numero_1)
    optimizacion.optimizar_programa(programa)(0) should equal(numero_1)
  }
  it("No se optimizar  el Booleano LTrue"){
    var boolean_LTrue = LTrue()
    var programa:Array[Instruccion]   = Array(boolean_LTrue)
    optimizacion.optimizar_programa(programa)(0) should equal(boolean_LTrue)
  }
  it("No se optimizar el Booleano LFalse"){
    var boolean_LFalse = LFalse()
    var programa:Array[Instruccion]   = Array(boolean_LFalse)
    optimizacion.optimizar_programa(programa)(0) should equal(boolean_LFalse)
  }
  it("No se optimiza mas la Variable"){
    var variable = Variable("pepe")
    var programa:Array[Instruccion] = Array(variable)
    optimizacion.optimizar_programa(programa)(0) should equal(variable)
  }

  //Casos de operaciones Aritmeticas
  //Suma
  it("Optimiza la suma con cero a la derecha") {
     var optimizacion:Optimizacion = new Optimizacion();
     var numero_1 = Numero(1);
     var numero_2 = Numero(0);
     var suma            = Suma(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(suma)
     optimizacion.optimizar_programa(programa)(0) should equal(numero_1)

   }

  it("Optimiza la suma con cero a la izquierda") {
    var numero_1 = Numero(0);
    var numero_2 = Numero(1);
    var suma            = Suma(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(suma)
    optimizacion.optimizar_programa(programa)(0) should equal(numero_2)

  }
   //Resta
   it("Optimizo la resta con cero a la derecha"){
     var numero_1 = Numero(1)
     var numero_2 = Numero(0)
     var resta           = Resta(numero_1,numero_2)
     var programa:Array[Instruccion]   = Array(resta)
     optimizacion.optimizar_programa(programa)(0) should equal(numero_1)
   }


   //Multiplicacion
   it("Optimizo la operacion multiplicacion con 1 a la izquierda"){
     var numero_1 = Numero(1)
     var numero_2 = Numero(2)
     var multiplicacion  = Multiplicacion(numero_1,numero_2)
     var programa:Array[Instruccion]   = Array(multiplicacion)
     optimizacion.optimizar_programa(programa)(0) should equal(numero_2)
   }

  it("Optimizo la operacion multiplicacion con 1 a la derecha"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(2)
    var multiplicacion  = Multiplicacion(numero_2,numero_1)
    var programa:Array[Instruccion]   = Array(multiplicacion)
    optimizacion.optimizar_programa(programa)(0) should equal(numero_2)
  }
   //Division
   it("Optimizo la operacion de division"){
     var numero_1 = Numero(3)
     var numero_2 = Numero(1)
     var division        = Division(numero_1,numero_2)
     var programa:Array[Instruccion]   = Array(division)
     optimizacion.optimizar_programa(programa)(0) should equal(numero_1)
    }


   it("No se optimizar operacion suma"){
     var numero_1 = Numero(1)
     var numero_2 = Numero(1)
     var suma            = Suma(numero_1,numero_2)
     var programa:Array[Instruccion]   = Array(suma)
     optimizacion.optimizar_programa(programa)(0) should equal(suma)
   }

  //Casos de operaciones Booleanas
  //Menor
  it("Optimizo la operacion booleana menor"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(5)
    var menor    = Menor(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(menor)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }


  //Mayor
  it("Optimizo la operacion booleana mayor"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(5)
    var mayor    = Mayor(numero_2,numero_1)
    var programa:Array[Instruccion]   = Array(mayor)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }

  //IGual
  it("Optimizo la operacion booleana igual"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(5)
    var igual    = Igual(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())
  }

  //MayorIgual
  it("Optimizo la operacion booleana MayorIgual,numero no es mayor pero es igual"){
    var numero_1 = Numero(5)
    var numero_2 = Numero(5)
    var mayor_igual    = MayorIgual(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(mayor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  it("Optimizo la operacion MayorIgual,numero no es igual pero es mayor"){
    var numero_1    = Numero(10)
    var numero_2    = Numero(5)
    var mayor_igual = MayorIgual(numero_1,numero_2)
    var programa:Array[Instruccion] = Array(mayor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  it("Optimizo la operacion MayorIgual, numero no es igual ni mayor"){
    var numero_1    = Numero(10)
    var numero_2    = Numero(5)
    var mayor_igual = MayorIgual(numero_2,numero_1)
    var programa:Array[Instruccion] = Array(mayor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())
  }
  //MenorIgual
  it("Optimizo la operacion booleana MenorIgual,numero no es menor pero es igual"){
    var numero_1                      = Numero(5)
    var numero_2                      = Numero(5)
    var menor_igual                   = MenorIgual(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(menor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  it("Optimizo la operacion booleana MenorIgual, numero no es igual pero es menor"){
    var numero_1                      = Numero(1)
    var numero_2                      = Numero(5)
    var menor_igual                   = MenorIgual(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(menor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  it("Optimizo la operacion booleana MenorIgual, numero no es igual ni menor"){
    var numero_1                      = Numero(10)
    var numero_2                      = Numero(5)
    var menor_igual                   = MenorIgual(numero_1,numero_2)
    var programa:Array[Instruccion]   = Array(menor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())
  }

  //Casos recursivos
  //Suma
  it("Optimizo operacion Suma con otra operacion Aritmetica como hijo"){
    var numero_1 = Numero(1);
    var numero_2 = Numero(0)
    var suma_1   = Suma(numero_1,numero_2)
    var suma_2   = Suma(numero_2 ,suma_1)
    var programa:Array[Instruccion]   = Array(suma_2)
    optimizacion.optimizar_programa(programa)(0) should equal(numero_1)

  }
  //Resta
  it("Optimizo operacion Resta con otra operacion aritmetica como hijo"){
    var numero_1  = Numero(1);
    var numero_2  = Numero(0)
    var suma      = Suma(numero_1,numero_2)
    var resta_2   = Resta(Numero(1),suma)
    var programa:Array[Instruccion]   = Array(resta_2)
    var result_esperado               = Resta(numero_1,numero_1)
    optimizacion.optimizar_programa(programa)(0) should equal(result_esperado)
  }
  //Multiplicacion
  it("Optimizo operacion Multiplicacion con otra operacion Aritmetica"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(0)
    var division = Division(numero_2,numero_1)
    var multiplicacion = Multiplicacion(Numero(1),division)
    var programa:Array[Instruccion]   = Array(multiplicacion)
    optimizacion.optimizar_programa(programa)(0) should equal(numero_2)

  }
  //Division
  it("Optimizo operacion Division con otra operacion Aritmetica"){
    var numero_1       = Numero(1)
    var numero_2       = Numero(0)
    var multiplicacion = Multiplicacion(numero_1,numero_2)
    var suma           = Suma(numero_1,numero_2)
    var division       = Division(suma,multiplicacion)
    var programa:Array[Instruccion]   = Array(division)
    var result_esperado = Division(numero_1,numero_2)
    optimizacion.optimizar_programa(programa)(0) should equal(result_esperado)
  }
  //Menor
  it("Optimizo operacion Booleana Menor, que tiene como hijos Operaciones Aritmeticas"){
    var numero_1 = Numero(1)
    var numero_2 = Numero(0)
    var suma     = Suma(numero_1,numero_2)
    var division = Division(numero_2,numero_1)
    var menor    = Menor(suma,division)
    var programa:Array[Instruccion]   = Array(menor)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())
  }
  //Mayor
  it("Optimizo operacion Booleana Mayor, que tiene como hijos operaciones Aritmeticas"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(0)
    var suma                        = Suma(numero_1,numero_2)
    var resta                       = Resta(numero_1,numero_2)
    var mayor                       = Mayor(suma,resta)
    var programa:Array[Instruccion] = Array(mayor)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())

  }
  //Igual
  it("Optimzio Operacion booleana Igual con otras operacion booleana como hijo"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var menor                       = Menor(numero_1,numero_2)
    var mayor                       = Mayor(numero_2,numero_1)
    var igual                       = Igual(menor,mayor)
    var programa:Array[Instruccion] = Array(igual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  //MenorIgual
  it("Optimizo Operacion boolena MenorIgual con otras operaciones como hijo "){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(0)
    var suma                        = Suma(numero_1,numero_2)
    var multiplicacion              = Multiplicacion(numero_1,numero_2)
    var menorIgual                  = MenorIgual(multiplicacion,suma)
    var programa:Array[Instruccion] = Array(menorIgual)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  //MayorIgual
  it("Optimizo la operacion MayorIgual con operaciones como hijo"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(0)
    var suma                        = Suma(numero_1,numero_2)
    var multiplicacion              = Multiplicacion(numero_1,numero_2)
    var mayorIgual                  = MayorIgual(multiplicacion,suma)
    var programa:Array[Instruccion] = Array(mayorIgual)
    optimizacion.optimizar_programa(programa)(0) should equal(LFalse())
  }
  //Operaciones Booleanas con hijos de tipo Booleana
  //Menor
  it("Optimizo operacion Booleana Menor con hijos de tipo Booleano"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var igual                       = Igual(numero_1,numero_2)
    var mayor                       = Mayor(numero_1,numero_2)
    var menor                       = Menor(mayor,igual)
    var programa:Array[Instruccion] = Array(menor)
    var result_expected             = Menor(LFalse(),LFalse())
    optimizacion.optimizar_programa(programa)(0) should equal(result_expected)
  }
  //Mayor
  it("Optimizo operacion Booleana Mayor con hijos de tipo Booleano"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var igual                       = Igual(numero_1,numero_2)
    var menor                       = Menor(numero_1,numero_2)
    var mayor                       = Mayor(menor,igual)
    var programa:Array[Instruccion] = Array(mayor)
    var result_expected             = Mayor(LTrue(),LFalse())
    optimizacion.optimizar_programa(programa)(0) should equal(result_expected)
  }
  //Igual
  it("Optimizo operacion Booleana Igual con hijos de tipo Booleano"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var menor                       = Menor(numero_1,numero_2)
    var mayor                       = Mayor(numero_1,numero_2)
    var igual                       = Igual(mayor,menor)
    var programa:Array[Instruccion] = Array(igual)
    var result_expected             = LFalse()
    optimizacion.optimizar_programa(programa)(0) should equal(result_expected)
  }
  //MenorIgual
  it("Optimizo operacion Booleana MenorIgual con hijos de tipo Booleano"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var igual                       = Igual(numero_1,numero_2)
    var mayor                       = Mayor(numero_1,numero_2)
    var menorIgual                  = MenorIgual(mayor,igual)
    var programa:Array[Instruccion] = Array(menorIgual)
    var result_expected             = MenorIgual(LFalse(),LFalse())
    optimizacion.optimizar_programa(programa)(0) should equal(result_expected)
  }
  //MayorIgual
  it("Optimizo operacion Booleana MayorIgual con hijos de tipo Booleano"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(2)
    var igual                       = Igual(numero_1,numero_2)
    var mayor                       = Mayor(numero_1,numero_2)
    var mayorIgual                  = MayorIgual(mayor,igual)
    var programa:Array[Instruccion] = Array(mayorIgual)
    var result_expected             = MayorIgual(LFalse(),LFalse())
    optimizacion.optimizar_programa(programa)(0) should equal(result_expected)
  }
  //Mayor
  it("No se optimiza mas operacion booleana Mayor"){
    var numero_1 = Numero(5)
    var numero_2 = Numero(5)
    var resta     = Resta(numero_1,numero_2)
    var multiplicacion = Multiplicacion(numero_2,numero_1)
    var mayor    = Mayor(resta,multiplicacion)
    var programa:Array[Instruccion]   = Array(mayor)
    optimizacion.optimizar_programa(programa)(0) should equal(mayor)
  }

  it("No se optimiza operacion booleana igual"){
    var numero_1          = Numero(5)
    var numero_2          = Numero(5)
    var suma    = Suma(numero_1,numero_2)
    var multiplicacion    = Multiplicacion(numero_2,numero_1)
    var igual             = Igual(suma,multiplicacion)
    var programa:Array[Instruccion]   = Array(igual)
    optimizacion.optimizar_programa(programa)(0) should equal(igual)
  }
  it("No se optimiza operacion booleana mayorIgual"){
    var numero_1 = Numero(5)
    var numero_2 = Numero(5)
    var suma     = Suma(numero_1,numero_2)
    var division = Division(numero_2,numero_1)
    var mayor_igual    = MayorIgual(suma,division)
    var programa:Array[Instruccion]   = Array(mayor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(mayor_igual)
  }
  it("No se optimiza operacion booleana menor_Igual"){
    var numero_1       = Numero(5)
    var numero_2       = Numero(5)
    var suma           = Suma(numero_1,numero_2)
    var division       = Division(numero_2,numero_1)
    var menor_igual    = MenorIgual(suma,division)
    var programa:Array[Instruccion]   = Array(menor_igual)
    optimizacion.optimizar_programa(programa)(0) should equal(menor_igual)
  }
  //Operacion Aritmeticas con valores Booleanas como hijos
  //Suma
  it("Optimizo la operacion Aritmetica Suma, con operacion booleana Menor"){
    var numero_1                    = Numero(0)
    var numero_2                    = Numero(1)
    var menor                       = Menor(numero_1,numero_2)
    var suma                        = Suma(numero_1,menor)
    var programa:Array[Instruccion] = Array(suma)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  //Resta
  it("Optimizo la operacion Aritmetica Resta con operaciones Booleana"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(1)
    var menorIgual                  = MenorIgual(numero_1,numero_2)
    var igual                       = Igual(numero_1,numero_2)
    var resta                       = Resta(menorIgual,igual)
    var programa:Array[Instruccion] = Array(resta)
    var result_expeted              = Resta(LTrue(),LTrue())
    optimizacion.optimizar_programa(programa)(0) should equal(result_expeted)
  }
  //Multiplicacion
  it("Optimizo la operacion Aritmetica Multiplicacion con operacion Booleana"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(1)
    var mayorIgual                  = MayorIgual(numero_1,numero_2)
    var multiplicacion              = Multiplicacion(numero_1,mayorIgual)
    var programa:Array[Instruccion] = Array(multiplicacion)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }
  //Division
  it("Optimizo la operacion Aritmetica Division con operacion Booleana"){
    var numero_1                    = Numero(1)
    var numero_2                    = Numero(1)
    var menorIgual                  = MenorIgual(numero_1,numero_2)
    var division                    = Division(menorIgual,numero_1)
    var programa:Array[Instruccion] = Array(division)
    optimizacion.optimizar_programa(programa)(0) should equal(LTrue())
  }

  //Casos de Variables
  //Suma
  it("Optimizo Declaracion de variable sin usar en operacion aritmetica  suma"){
    var declaracion_variable_pepe     = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual = DeclararVariable("añoActual", Numero(2020))
    var suma                          = Suma(Numero(1),Numero(2))
    var programa:Array[Instruccion]   = Array(declaracion_variable_pepe,declarar_variable_anio_actual,suma)
    optimizacion.optimizar_programa(programa)(0) should equal(suma)
  }
  //Resta
  it("Optimizo Declaracion de variable sin usar en operacion aritmetica resta"){
    var declaracion_variable_pepe     = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual = DeclararVariable("añoActual", Numero(2020))
    var resta                         = Resta(Variable("pepe"), Numero(25))
    var programa:Array[Instruccion]   = Array(declaracion_variable_pepe,declarar_variable_anio_actual,resta)
    optimizacion.optimizar_programa(programa).size should equal(2)
  }
  //Multiplicacion
  it("Optimizo Declaracion de variable sin usar en operacion aritmetica multiplicacion") {
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var multiplicacion                  = Multiplicacion(Numero(1),Numero(2))
    var programa:Array[Instruccion]     = Array(declaracion_variable_pepe,declarar_variable_anio_actual,multiplicacion)
    optimizacion.optimizar_programa(programa)(0) should equal(Numero(2))
  }
  //Division
  it("Optimizo Declaracion de variable sin usar en la operacion aritmetica division"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var division                        = Division(Numero(1),Numero(3))
    var programa:Array[Instruccion]     = Array(declaracion_variable_pepe,declarar_variable_anio_actual,division)
    optimizacion.optimizar_programa(programa).size should equal(1)

  }
  //Menor
  it("Optimizo Declaracion de variable sin usar en operacion Booleana menor"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var menor                           = Menor(Variable("pepe"),Numero(2))
    var programa:Array[Instruccion]     = Array(declaracion_variable_pepe,declarar_variable_anio_actual,menor)
    optimizacion.optimizar_programa(programa).size should equal(2)
  }
  //Mayor
  it("Optimizo Declaracion de variable sin usar en operacion Booleana Mayor"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var mayor                           = Mayor(Variable("pepe"),Numero(2))
    var programa:Array[Instruccion]     = Array(declaracion_variable_pepe,declarar_variable_anio_actual,mayor)
    optimizacion.optimizar_programa(programa).size should equal(2)
  }
  //Igual
  it("Optimizo Declaracion de variable sin usar en operacion Booleana Igual"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var igual                           = Igual(Variable("pepe"),Numero(2))
    var programa:Array[Instruccion]     = Array(declarar_variable_anio_actual,declaracion_variable_pepe,igual)
    optimizacion.optimizar_programa(programa).size should equal(2)
  }
  //MayorIgual
  it("Optimizo Declaracion de variavle sin usar  en operacion Booleana MayorIgual"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var mayor_igual                     = MayorIgual(Variable("pepe"),Numero(2))
    var programa:Array[Instruccion]     = Array(declarar_variable_anio_actual,declaracion_variable_pepe,mayor_igual)
    optimizacion.optimizar_programa(programa).size should equal(2)
  }
  //MenorIgual
  it("Optimizo Declaracion de variable sin usar en en operacion Booleana MenorIgual"){
    var declaracion_variable_pepe       = DeclararVariable("pepe", Numero(1))
    var declarar_variable_anio_actual   = DeclararVariable("añoActual", Numero(2020))
    var menor_igual                     = MenorIgual(Variable("pepe"),Numero(2))
    var programa:Array[Instruccion]     = Array(declarar_variable_anio_actual,declaracion_variable_pepe,menor_igual)
    optimizacion.optimizar_programa(programa).size should equal(2)

  }
  //Asignar
  it("Optimizo la operacion aritmetica Suma dentro de operacion Asignar"){
    var suma                          = Suma(Numero(1),Numero(0))
    var declaracion                   = DeclararVariable("hola mundo",Numero(1))
    var asignar                       = Asignar(Variable("hola mundo"),suma)
    var result_Expected               = Asignar(Variable("hola mundo"),Numero(1))
    var programa:Array[Instruccion]   = Array(declaracion,asignar)
    optimizacion.optimizar_programa(programa)(1) should equal(result_Expected)
  }

  it("Optimizo operacion booleana Menor dentro de operacion Asignar"){
    var menor                         = Menor(Numero(1),Numero(0))
    var declaracion                   = DeclararVariable("hola mundo",Numero(1))
    var asignar                       = Asignar(Variable("hola mundo"),menor)
    var programa:Array[Instruccion]   = Array(declaracion,asignar)
    var result_Expected               = Asignar(Variable("hola mundo"),LFalse())
    optimizacion.optimizar_programa(programa)(1) should equal(result_Expected)
  }

  it("Optimizo declaracion de variable sin usar en el asignar"){
    var declararVariable = DeclararVariable("Un valor",Numero(1))
    var asignar          = Asignar(Variable("pepa"),Numero(1))
    var programa:Array[Instruccion]  = Array(declararVariable,asignar)
    optimizacion.optimizar_programa(programa)(0) should equal(asignar)
  }
}
