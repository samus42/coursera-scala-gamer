val l = List("a", "b", "c")
import scala.util.Random

for (i <- 1 to 20) {
    println(scala.util.Random.nextInt(3))
}

val t = Set()

def mat(x: Any) = {
    x match {
        case Some(s: String) => s + "1"
        case Some(i: Int) => i * i
        case None => ""
    }
}

mat(Some("s"))
mat(Some(1))
