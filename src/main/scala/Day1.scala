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

  def loop(input: Vector[Int]): Int = {
    11
  }

  def stream(input: Vector[Int], target: Int): Option[Int] = {
    def stream: LazyList[Vector[Int]] = LazyList.iterate(input){
      l => if (l.head + l.last > target) l.init else l.tail
    }

    stream.take(input.size - 1)
      .map(l => (l.head,  l.last))
      .find {case (x, y) => x + y == target}
      .map{x => x._1 * x._2}
      .getOrElse({ return None})
  }

  def recursive(input: Vector[Int], target: Int): Int = {
    // tail, last, init, head
    val sorted: Vector[Int] = input.sorted
    def findPair(a: Vector[Int], b: Vector[Int], target: Int): Int = {
      if (a.head >= b.last) 0
      else if (a.head + b.last == target) a.head * b.last
      else if (a.head + b.last < target) findPair(a.tail, b, target)
      else findPair(a, b.init, target)
    }
    findPair(sorted, sorted.tail, target)
  }

  def bst(input: Vector[Int]): Option[Int] = {
    val x = 5

    try {
      Some(x + 1)
    } catch {
      case e => None
    }
  }
}
