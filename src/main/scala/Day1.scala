import Util.readInputInt

object Day1 {
  def main(): Unit = {
    val input: Vector[Int] = readInputInt(1)
    /*val input: Vector[Int] = Vector(1920, 300, 1520, 100, 444, 500, 700)*/
    // naive solution: for each element in the list, match with all other n-1 values until a match yields sum of 2020
    // O(n^2)

    val target: Int = 2020

    benchmark(naive, "naive", input, target)
    benchmark(recursive, "recursive", input, target)
    benchmark(stream, "stream", input, target)
    benchmark(triplet_map, "triplet_map", input, target)
    benchmark(triplet_recursive, "triplet_recursive", input, target)
  }

  def benchmark(callable: (Vector[Int], Int) => Int, name: String,  input: Vector[Int], target: Int): Double = {
    val start = System.nanoTime
    val res: Int = callable(input, target)
    val runtime = (System.nanoTime - start) / 1e9d
    println(s"$name: $res \t $runtime")
    runtime
  }

  def naive(input: Vector[Int], target: Int): Int = {
    input.zipWithIndex.map(outer =>
      input.zipWithIndex.find(inner => inner._1 == target - outer._1).map(inner => Vector(inner._2, outer._2).sorted)
    ).flatten.distinct.map(pair => input(pair(0)) * input(pair(1)))(0)
  }

  def stream(input: Vector[Int], target: Int): Int = {

    def stream: LazyList[Vector[Int]] = LazyList.iterate(input.sorted){
      l => if (l.head + l.last > target) l.init else l.tail
    }

    stream.take(input.size - 1)
      .map(l => (l.head,  l.last))
      .find {case (x, y) => x + y == target}
      .map{x => x._1 * x._2}.getOrElse(0)
  }

  def findPair(a: Vector[Int], b: Vector[Int], target: Int): Tuple2[Option[Int], Option[Int]] = {
    if (b.length == 0) Tuple2(None, None)
    else if (a.head >= b.last) Tuple2(None, None)
    else if (a.head + b.last == target) Tuple2(Some(a.head), Some(b.last))
    else if (a.head + b.last < target) findPair(a.tail, b, target)
    else findPair(a, b.init, target)
  }

  def recursive(input: Vector[Int], target: Int): Int = {
    val sorted: Vector[Int] = input.sorted
    val (a: Option[Int], b: Option[Int]) = findPair(sorted, sorted.tail, target)
    a.getOrElse(0) * b.getOrElse(0)
  }

  def findTriplet(vec: Vector[Int], target: Int) : (Option[Int], Option[Int], Option[Int]) = {
    if (vec.length == 0) return Tuple3(None, None, None)
    val (a, b) = findPair(vec, vec.tail, target - vec.head)
    if (a.getOrElse(0) + b.getOrElse(0) + vec.head > target) Tuple3(None, None, None)
    else if (a.getOrElse(0) + b.getOrElse(0) + vec.head == target) Tuple3(a, b, Some(vec.head))
    else findTriplet(vec.tail, target)
  }

  def triplet_recursive(input: Vector[Int], target: Int): Int = {
    val sorted: Vector[Int] = input.sorted
    val triplet = findTriplet(sorted, target)
    triplet._1.getOrElse(0) * triplet._2.getOrElse(0) * triplet._3.getOrElse(0)
  }

  def triplet_map(input: Vector[Int], target: Int): Int = {
    val sorted: Vector[Int] = input.sorted
    sorted.map(e => {
      val (a: Option[Int], b: Option[Int]) = findPair(sorted, sorted.tail, target - e)
      a.getOrElse(0) * b.getOrElse(0) * e
    }).find(_ != 0).getOrElse(0)

  }
}
