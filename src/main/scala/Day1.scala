import Util.readInputInt

object Day1 {
  def main(): Unit = {
    val input: Vector[Int] = readInputInt(1)
    /*val input: Vector[Int] = Vector(1920, 300, 1520, 100, 444, 500, 700)*/
    // naive solution: for each element in the list, match with all other n-1 values until a match yields sum of 2020
    // O(n^2)

    val t1 = System.nanoTime
    val naiveSol: Int = naive(input)
    val duration = (System.nanoTime - t1) / 1e9d
    println(s"naive: $naiveSol \t $duration")

    val t2 = System.nanoTime
    val loopSol: Int = loop(input)
    val d2 = (System.nanoTime - t2) / 1e9d
    println(s"loop: $loopSol \t $d2")

    // enhanced solution: for each element, check the difference up until 2020
  }

  def naive(input: Vector[Int]): Int = {
    val targetSum: Int = 2020
      input.zipWithIndex.map(outer =>
      input.zipWithIndex.find(inner => inner._1 == targetSum - outer._1).map(inner => Vector(inner._2, outer._2).sorted)
    ).flatten.distinct.map(pair => input(pair(0)) * input(pair(1)))(0)
  }

  def loop(input: Vector[Int]): Int = {
    var 
    input.takeWhile()
    for(x <- input.zipWithIndex){

    }
    5
  }

  def bst(input: Vector[Int]): Int = {
    5
  }
}
