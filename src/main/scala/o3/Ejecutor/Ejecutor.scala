package o3.Ejecutor

import o3.Expecion.{UndefinedVariable,OperationNonsense,NotCanRedefinedType,DuplicateVariable}
import o3.IF.If
import o3.Operaciones.{Division, Igual, Instruccion, Mayor, MayorIgual, Menor, MenorIgual, Multiplicacion, Resta, Suma}
import o3.Valor.{LFalse, LTrue, Numero, ValorBooleano, ValorNumerico}
import o3.Variable.{DeclararVariable, Variable, Asignar}

import scala.collection.mutable

class Ejecutor {
  var stack_variables_numericas:mutable.HashMap[String,Int]=mutable.HashMap();
  var stack_variables_boleanas:mutable.HashMap[String,Boolean]=mutable.HashMap();
  //Devuelve resultado de ejecutar la ultima instruccion
  def ejecutar_programa(programa : Array[Instruccion]): AnyVal = {
    var result:Array[AnyVal] = Array()
    try{
      programa.foreach( (currentInstruction : Instruccion) => {
        result =  ejecutar_AST(currentInstruction)
      })
      result(0)
    }
    catch{
      case a=> throw a
    }
    finally {
      this.stack_variables_numericas=mutable.HashMap()
      this.stack_variables_boleanas=mutable.HashMap()
    }

  }

  def ejecutar_AST(un_AST:Instruccion): Array[AnyVal] ={
    val result_Either:Either[Int,Boolean] = either_ejecutor(un_AST)
    if(result_Either.isLeft){
      Array(result_Either.left.get)
    }
    else {
      Array(result_Either.right.get)
    }
  }
  def either_ejecutor(un_AST:Instruccion):Either[Int,Boolean] = un_AST match {

    //Casos bases
    case Numero(n)                            => new Left(n);
    case LTrue()                              => new Right(true)
    case LFalse()                             => new Right(false)

    //Casos de operaciones Aritmeticas
    case Suma(Numero(n), Numero(m))           => new Left(n + m);
    case Resta(Numero(n), Numero(m))          => new Left(n - m);
    case Multiplicacion(Numero(n), Numero(m)) => new Left(n * m);
    case Division(Numero(n), Numero(m))       => new Left(n / m);

    //Casos de operaciones Booleanas
    case Menor(Numero(n), Numero(m))          => new Right(n < m);
    case Mayor(Numero(n), Numero(m))          => new Right(n > m);
    case Igual(Numero(n), Numero(m))          => new Right(n == m);
    case MayorIgual(Numero(n), Numero(m))     => new Right(n >= m);
    case MenorIgual(Numero(n), Numero(m))     => new Right(n <= m);

    //Declaracion de Variables
    case DeclararVariable(nombre,valor) =>{
      if(this.stack_variables_numericas.contains(nombre) || this.stack_variables_boleanas.contains(nombre)){
        throw new DuplicateVariable("La variable ya se definio")
      }
      val result = this.either_ejecutor(valor)
      if(result.isLeft) {
        this.stack_variables_numericas = this.stack_variables_numericas + (nombre -> result.left.get)
        result
      }
      else {
        this.stack_variables_boleanas = this.stack_variables_boleanas + (nombre -> result.right.get)
        result
      }
    }

    //Lectura de variables
    case Variable(nombre) => if (this.stack_variables_boleanas.contains(nombre)){
      new Right(this.stack_variables_boleanas(nombre))}
    else if(this.stack_variables_numericas.contains(nombre)){
      new Left(this.stack_variables_numericas(nombre))
    }else{
       throw new UndefinedVariable("la variable nunca se declaro")
    }

    //Escritura de variables
    case Asignar(Variable(name_variable), operacion) =>{
      val get_var=this.either_ejecutor(Variable(name_variable))
      val result = this.either_ejecutor(operacion)

      if(result.isLeft && get_var.isLeft){
        this.stack_variables_numericas.update(name_variable,result.left.get)
        new Left(result.left.get)
      }else if(result.isRight && get_var.isRight){
        this.stack_variables_boleanas.update(name_variable,result.right.get)
        new Right(result.right.get)
      }else{
        throw new NotCanRedefinedType("No se puede cambiar el tipo de la variable")
      }
    }

    //Casos recursion operacion Aritmerica
    case Suma(numerio_1, numerio_2)  => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(numerio_1),either_ejecutor(numerio_2))
      new Left(tupla._2 + tupla._1)
    };
    case Resta(numerio_1, numerio_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(numerio_1),either_ejecutor(numerio_2))
      new Left(tupla._1 - tupla._2)
    }
    case Multiplicacion(numerio_1, numerio_2)   => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(numerio_1),either_ejecutor(numerio_2))
      new Left(tupla._1 * tupla._2)

    }
    case Division(numerio_1, numerio_2) =>{
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(numerio_1),either_ejecutor(numerio_2))
      new Left(tupla._1 / tupla._2)
    }
    //Casos recursion operacion Booleana
    case Menor(valor_1, valor_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(valor_1),either_ejecutor(valor_2))
      new Right(tupla._1 < tupla._2)
    }
    case Mayor(valor_1, valor_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(valor_1),either_ejecutor(valor_2))
      new Right(tupla._1 > tupla._2)
    }
    case Igual(valor_1, valor_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(valor_1),either_ejecutor(valor_2))
      new Right(tupla._1 == tupla._2)
    }
    case MenorIgual(valor_1, valor_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(valor_1),either_ejecutor(valor_2))
      new Right(tupla._1 <= tupla._2)
    }
    case MayorIgual(valor_1, valor_2) => {
      val tupla = this.obtener_valores_si_corresponde(either_ejecutor(valor_1),either_ejecutor(valor_2))
      new Right(tupla._1 >= tupla._2)
    }

  }
  def obtener_valores_si_corresponde(val_1:Either[Int,Boolean],val_2:Either[Int,Boolean]): (Int,Int) ={
    if(val_1.isLeft && val_2.isLeft){
      (val_1.left.get,val_2.left.get)
    }else{
        throw new OperationNonsense("Operacion sin sentido")
    }
  }

}


