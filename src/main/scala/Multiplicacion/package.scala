package object Multiplicacion {
  def PeasantAlgorithm(a: Int, b: Int): Int = {
    // Este es nuestro caso base, si el multiplicador "a" es 0, el producto es 0
    if (a == 0) 0 else {
    /* Si a es impar sumamos 'b' al resultado y realizamos
       la llamada recursiva dividiendo 'a' a la mitad (a / 2) y duplicando "b"*/
      if (a % 2 != 0) b + PeasantAlgorithm(a / 2, b + b)
      /* Si 'a' es par, no sumamos nada en este nivel y continuamos
       la reducción con la mitad de "a" y el doble de "b" */
      else PeasantAlgorithm(a / 2, b + b)
    }
  }
    
}

def PeasantAlgorithmIt(a: Int, b: Int): Int = {
  def auxIter(a: Int, b: Int, accumulator: Int): Int = {
    // Nuestro Caso base aqui es Cuando 'a' llega a 0, retornamos directamente el valor acumulado
    if (a == 0) accumulator
    else {
      // Si 'a' es impar, avanzamos sumando 'b' al acumulador (accumulator + b)
      if (a % 2 != 0) auxIter(a / 2, b + b, accumulator + b)
      // Si 'a' es par, avanzamos manteniendo el acumulador intacto
      else auxIter(a / 2, b + b, accumulator)
  }
}
  // Este es nuestro punto de entrada donde invocamos la función auxiliar iniciando el acumulador en 0
  auxIter(a, b, 0)
}



def splitMultiply(a: Int, b: Int): Int = {
  // numDigits en nuestro programa es una función auxiliar para contar los digitos de un numero
  def numDigits(n: Int): Int = {
    if (n < 10) 1 else 1 + numDigits(n / 10)
  }
  /*Este es nuestro caso base, permitido por el enunciado del taller donde
  si ambos números son mayores o iguales a 0 (para garantizar que no se use la multiplicacion de scala de forma prohibida)
   y menores a 10 osea son de una cifra, simplemente multiplicamos ab con el operador (*) */
  if (a >= 0 && a < 10 && b >= 0 && b < 10) a * b else {
    val n = if (numDigits(a) > numDigits(b)) numDigits(a) else numDigits(b)
    val m = n / 2
    val p = math.pow(10, m).toInt
  // Descomposicion en partes
    val x = a / p
    val y = a % p
    val z = b / p
    val w = b % p
    // Llamadas recursivas
    val xz = splitMultiply(x, z)
    val yz = splitMultiply(y, z)
    val xw = splitMultiply(x, w)
    val yw = splitMultiply(y, w)
    //Reconstruccion final usando la formula del enunciado del taller
    (xz * math.pow(10, m+m).toInt) + ((yz + xw) * math.pow(10, m).toInt) + yw


  }


}

def fastMultiply(a: Int, b: Int): Int = {
  // numDigits en nuestro programa es una función auxiliar para contar los digitos de un numero
  def numDigits(n: Int): Int = {
    if (n < 10) 1 else 1 + numDigits(n / 10)
  }
  /*Este es nuestro caso base, permitido por el enunciado del taller donde
  si ambos números son mayores o iguales a 0 (para garantizar que no se use la multiplicacion de scala de forma prohibida)
   y menores a 10 osea son de una cifra, simplemente multiplicamos ab con el operador (*) */
  if (a >= 0 && a < 10 && b >= 0 && b < 10) a * b else {
    val n = if (numDigits(a) > numDigits(b)) numDigits(a) else numDigits(b)
    val m = n / 2
    val p = math.pow(10, m).toInt
    //Descomposicion en partes
    val x = a / p
    val y = a % p
    val z = b / p
    val w = b % p
    //Garantizamos que diffx y diffz sean >= 0 para cumplir con la restriccion
    val diffx = if (x >= y) x - y else y - x
    val diffz = if (z >= w) z - w else w - z
    // 3 multiplicaciones recursivas
    val xz = fastMultiply(x, z)
    val yw = fastMultiply(y, w)
    val prodDiff = fastMultiply(diffx, diffz)
    // Si los signos de (x-y) y (z-w) eran iguales se resta prodDiff, si eran opuestos se suma
    val centralTerm = if ((x >= y) == (z >= w)) xz + yw - prodDiff
    else xz + yw + prodDiff
    //Reconstruccion final usando la formula del enunciado del taller
    (xz * math.pow(10,  m+m).toInt) + (centralTerm * math.pow(10, m).toInt) + yw
  }

}











