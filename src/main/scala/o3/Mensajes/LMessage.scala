package o3.Mensajes

import o3.Operaciones.Instruccion

class LMessage(var message : String, var instruction: Instruccion) {}

class LWarning(message : String, instruction: Instruccion) extends LMessage(message, instruction)

class LError(message : String, instruction: Instruccion) extends LMessage(message, instruction)