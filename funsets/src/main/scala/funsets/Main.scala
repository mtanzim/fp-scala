package funsets

object Main extends App {

  import FunSets._

  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(1), 2))

  val sInt1 = intersect(singletonSet(1), singletonSet(3))
  println(contains(sInt1, 1))
  val sInt2 = intersect(singletonSet(1), singletonSet(1))
  println(contains(sInt2, 1))

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val sMisc = singletonSet(-34)
  val s = union(union(union(s1, s2), s3), sMisc)

  def p = (x: Int) => x == -55

  val resFilter = filter(s, p)
  printSet(resFilter)
  printSet(s)
  println(forall(s, (x) => x > 0))
  println(forall(s, (x) => x > -99))

  println(exists(s, (x) => x > 0))
  println(exists(s, (x) => x > -99))
  println(exists(s, (x) => x == -34))
  println(exists(s, (x) => x < -99))

  printSet(map(s, x => x * 2))

}
