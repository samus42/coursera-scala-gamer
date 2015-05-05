val visits = 2
val scores = Array(10.0,20.0)
val utilities = Array(30.0,40.0)

val c = utilities.zip(scores)
c.map {
    case (u, s) => u + (s - u) / visits
}