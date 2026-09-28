package object Multiplicacion {
  def PeasantAlgorithm(a: Int, b: Int): Int = {
    if (a == 0) {
      0
    } else if (a == 1) {
      b
    }
    else if (a % 2 != 0) {
      b + PeasantAlgorithm(a / 2, b + b)

    }
    else {
      PeasantAlgorithm(a / 2, b + b)
    }
  }
}

def PeasantAlgorithmIT(a: Int, b: Int): Int = {
  var x = a
  var y = b
  var resultado = 0

  while (x > 0) {
    if (x % 2 != 0) {
      resultado += y
    }


    x = x / 2
    y = y + y
  }


  resultado
}
def splitMultiply(a: Int, b: Int): Int = {
  // numDigits en nuestro programa es una función auxiliar para contar los digitos de un numero
  def numDigits(n: Int): Int = {
    if (n < 10) 1 else 1 + numDigits(n / 10)
  }
  /*Este es nuestro caso base, permitido por el enunciado del taller donde
  si ambos números son menores a 10 osea son de una cifra, simplemente multiplicamos ab con el operador (*) */
  if (a < 10 && b < 10) a * b else {
    val n = math.max(numDigits(a), numDigits(b))
    val m = n / 2
    val p = math.pow(10, m).toInt

    val x = a / p
    val y = a % p
    val z = b / p
    val w = b % p

    val xz = splitMultiply(x, z)
    val yz = splitMultiply(y, z)
    val xw = splitMultiply(x, w)
    val yw = splitMultiply(y, w)
    (xz * math.pow(10, 2 * m).toInt) + ((yz + xw) * math.pow(10, m).toInt) + yw


  }


}











