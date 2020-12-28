package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(1), 2))

  val sInt1 = intersect(singletonSet(1), singletonSet(3))
  println(contains(sInt1, 1))
  val sInt2 = intersect(singletonSet(1), singletonSet(1))
  println(contains(sInt2, 1))
}
