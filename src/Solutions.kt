fun main() {
    println("Максимальный палиндром, полученный перемножением 2-х 3-х значных чисел, полученный в котлине -> ${fourth()}")
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