import kotlin.math.abs

fun main() {
    println("Максимальный палиндром, полученный перемножением 2-х 3-х значных чисел, полученный в котлине -> ${fourth()}")
    println("(A, B, Max prime number) -> ${primeSolution()}")
}

fun fourth() :Int {
    var max :Int = 0
    for (i in 100..999) {
        for (j in 100..999) {
            if (i*j > max && i*j == reverseNumber(i*j)) {
                max = i*j
            }
        }
    }
    return max
}

val reverseNumber: (Int) -> Int = { x ->
    var reversed = 0
    var a = x
    while (a != 0) {
        val digit = a % 10
        reversed = reversed * 10 + digit
        a /= 10
    }
    reversed
}

fun primeSolution() :Triple<Int, Int, Int> {
    var bestNum = 0
    var bestA = 0
    var bestB = 0
    for (a in -1000..1000) {
        for (b in -1000..1000) {
            val num = numberOfPrimes(a, b)
            if (num > bestNum) {
                bestNum = num
                bestA = a
                bestB = b
            }
        }
    }
    return Triple(bestA, bestB, bestNum)
}

fun numberOfPrimes(a :Int, b:Int) :Int {
    val res: (Int, Int, Int) -> Int = {a, b, n -> n*n + n*a + b}
    for (i in 0..80) {
        if (!isPrime(res(a, b, i))) {
            return i
        }
    }
    return 0
}

fun isPrime(n :Int) :Boolean {
    val num = abs(n)
    if (num == 1 || num == 0) return false
    var i = 2
    while (i < num) {
        if (num % i == 0) {
            return false
        }
        i++
    }
    return true
}