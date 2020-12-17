package o3

import o3.Operaciones._
import o3.Analisis.{DivisionPorCeroMatcher, DivisionPorUnoMatcher, GlobalRule, IgualInvalidoMatcher, IgualRedundanteMatcher, LAnalyzer, LineRule, MultiplicacionPorUnoMatcher, MyError, MyWarning, RestaRedundanteMatcher, Rule, SumaRedundanteMatcher, VariableDeclaradaNoUsadaMatcher, VariableUsadaAntesDeSerDeclaradaMatcher, VariableUsadaNoDeclaradaMatcher, VariableYaDeclaradaMatcher}
import o3.Valor.{LFalse, LTrue, Numero}
import o3.Mensajes.{LError, LMessage, LWarning}
import o3.Variable.{DeclararVariable, Variable}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ASTAnalizadorSpec extends AnyFunSpec with Matchers {

  var analizador: LAnalyzer = new LAnalyzer()

  it("test de reglas") {
    val instruction : Instruccion = Suma(Numero(3), Numero(0));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Suma redundante", SumaRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)

  }


  it("Analisis de la suma con cero a la izquierda retorna una advertencia") {
    val instruction : Instruccion = Suma(Numero(0), Numero(1));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Suma redundante", SumaRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de la resta con cero a la derecha retorna una advertencia") {
    val instruction : Instruccion = Resta(Numero(1), Numero(0));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Resta redundante", RestaRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de la multiplicacion con uno a la izquierda retorna una advertencia") {
    val instruction : Instruccion = Multiplicacion(Numero(1), Numero(2));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Multiplicacion redundante", MultiplicacionPorUnoMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de la division por uno retorna una advertencia") {
    val instruction : Instruccion = Division(Numero(3), Numero(1));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Division redundante", DivisionPorUnoMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de la division por cero retorna un error") {
    val instruction : Instruccion = Division(Numero(3), Numero(0));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("No se puede dividir por cero", DivisionPorCeroMatcher, MyError())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de comparacion de numeros iguales") {
    val instruction : Instruccion = Igual(Numero(1), Numero(1));
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de comparacion true == true") {
    val instruction : Instruccion = Igual(LTrue(), LTrue());
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de comparacion true == false") {
    val instruction : Instruccion = Igual(LTrue(), LFalse());
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de comparacion entre un numero y un booleano") {
    val instruction : Instruccion = Igual(Numero(3), LFalse());
    val program : Array[Instruccion] = Array(instruction)


    val rule : Rule = new LineRule("Comparacion incorrecta", IgualInvalidoMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule))

    result.length should equal(1)
  }

  it("Analisis de una comparacion sin problemas, retorna lista vacia de mensajes") {
    val instruction : Instruccion = Igual( Suma( Numero(2), Numero(3) ), Resta( Numero(5), Numero(1) ) );
    val program : Array[Instruccion] = Array(instruction)


    val rule1 : Rule = new LineRule("Suma redundante", SumaRedundanteMatcher, MyWarning())
    val rule2 : Rule = new LineRule("Resta redundante", RestaRedundanteMatcher, MyWarning())
    val rule3 : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1, rule2, rule3))

    result.length should equal(0)
  }

  it("Analisis de una comparacion con 2 problemas, retorna lista con ambos mensajes") {
    val instruction : Instruccion = Igual( Suma( Numero(0), Numero(1) ), Resta( Numero(5), Numero(0) ) );
    val program : Array[Instruccion] = Array(instruction)


    val rule1 : Rule = new LineRule("Suma redundante", SumaRedundanteMatcher, MyWarning())
    val rule2 : Rule = new LineRule("Resta redundante", RestaRedundanteMatcher, MyWarning())
    val rule3 : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1, rule2, rule3))

    result.length should equal(2)
  }

  it("Analisis de suma utilizando recursion") {
    val instruction : Instruccion = Suma(Suma(Numero(54), Numero(0)), Numero(0));
    val program : Array[Instruccion] = Array(instruction)


    val rule1 : Rule = new LineRule("Suma redundante", SumaRedundanteMatcher, MyWarning())
    val rule2 : Rule = new LineRule("Resta redundante", RestaRedundanteMatcher, MyWarning())
    val rule3 : Rule = new LineRule("Comparacion redundante", IgualRedundanteMatcher, MyWarning())

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1, rule2, rule3))

    result.length should equal(2)
  }

  it("Declarar mas de una vez una variable retorna un warning") {
    val decl1 = DeclararVariable("valor1", Numero(3));
    val decl2 = DeclararVariable("valor2", Numero(4));
    val decl3 = DeclararVariable("valor1", Numero(2));
    val program : Array[Instruccion] = Array(decl1, decl2, decl3);

    val rule1 : Rule = new GlobalRule("Existe duplicacion de declaracion de variables", VariableYaDeclaradaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1));

    result.length should equal(1);
  }

  it("Declarar variables sin ser utilizadas provoca un warning de variables declaradas no usadas") {
    val decl1 = DeclararVariable("var1", Numero(3));
    val decl2 = DeclararVariable("var2", Numero(4));
    val decl3 = DeclararVariable("var3", Numero(2));
    val program : Array[Instruccion] = Array(decl1, decl2, decl3);

    val rule1 : Rule = new GlobalRule("Existe variables declaradas no utilizadas", VariableDeclaradaNoUsadaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1));

    result.length should equal(1);
  }

  it("Usar una variable despues de ser declarada no trae problemas") {
    val decl1 = DeclararVariable("v1", Numero(3))
    val decl2 = Igual(Suma(Variable("v1"), Numero(4)), Numero(7))
    val program : Array[Instruccion] = Array(decl1, decl2);

    val rule1 : Rule = new GlobalRule("Existe duplicacion de declaracion de variables", VariableYaDeclaradaMatcher, MyWarning());
    val rule2 : Rule = new GlobalRule("Existe variables declaradas no utilizadas", VariableDeclaradaNoUsadaMatcher, MyWarning());
    val rule3 : Rule = new GlobalRule("Existe variables usadas antes de ser declaradas", VariableUsadaAntesDeSerDeclaradaMatcher, MyWarning());
    val rule4 : Rule = new GlobalRule("Existe variables usadas no declaradas", VariableUsadaNoDeclaradaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1, rule2, rule3, rule4));

    result.length should equal(0);
  }

  it("Usar una variable que no fue declarada presenta un warning") {

    val decl1 = DeclararVariable("valor1", Numero(3));
    val decl2 = Igual(Suma(Variable("valor2"), Numero(4)), Numero(7));
    val program : Array[Instruccion] = Array(decl1, decl2);

    val rule4 : Rule = new GlobalRule("Existe variables usadas no declaradas", VariableUsadaNoDeclaradaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule4));

    result.length should equal(1);
  }


  it("Declarar una variable posterior a su uso presenta un warning particular") {
    val decl1 = Resta(Variable("añoActual"), Numero(25))
    val decl2 = DeclararVariable("añoActual", Numero(2020))
    val program : Array[Instruccion] = Array(decl1, decl2);

    val rule4 : Rule = new GlobalRule("Existe variables usadas antes de ser declaradas", VariableUsadaAntesDeSerDeclaradaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule4));

    result.length should equal(1);
  }

  it("Variables declaradas sin luego ser utilizadas, tmb presenta warnings") {
    val decl1 = DeclararVariable("pepe", Numero(1))
    val decl2 = DeclararVariable("añoActual", Numero(2020))
    val decl3 = Resta(Variable("añoActual"), Numero(25))
    val program : Array[Instruccion] = Array(decl1, decl2, decl3);

    val rule1 : Rule = new GlobalRule("Existe variables declaradas no utilizadas", VariableDeclaradaNoUsadaMatcher, MyWarning());

    val result : Array[LMessage] = analizador.analyze(program, Array(rule1));

    result.length should equal(1);
  }
}