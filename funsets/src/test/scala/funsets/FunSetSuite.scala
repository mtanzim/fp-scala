package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s34 = singletonSet(34)

  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   *
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */

      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(!contains(s1, 0), "Singleton not contains")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains intersecting elements of each set`: Unit = {
    new TestSets {
      val sa = intersect(s1, s2)
      assert(!contains(sa, 1), "Intersect 1,2")
      val sb = intersect(s1, s1)
      assert(contains(sb, 1), "Intersect 1,1")
    }
  }

  @Test def `diff contains diff elements of each set`: Unit = {
    new TestSets {
      val sa = union(s1, s2)
      val sb = union(s2, s3)
      val s = diff(sa, sb)
      assert(contains(s, 1), "Diff positive")
      assert(!contains(s, 2), "Diff negative")
    }
  }

  @Test def `filter set`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)

      def p = (x: Int) => x > 1

      val res = filter(s, p)
      assert(contains(res, 3), "filter positive")
      assert(contains(res, 2), "filter negative negative")
      assert(!contains(res, 1), "filter negative negative")
      assert(!contains(res, 45), "filter negative negative")

    }
  }

  @Test def `forall set`: Unit = {
    new TestSets {
      val s = union(union(union(s1, s2), s3), s34)
      assert(!forall(s, (x) => x > 1))
      assert(!forall(s, (x) => x == 1))
      assert(forall(s, (x) => x > 0))

    }
  }

  @Test def `exists set`: Unit = {
    new TestSets {
      val s = union(union(union(s1, s2), s3), s34)
      assert(exists(s, (x) => x > 1))
      assert(exists(s, (x) => x == 1))
      assert(exists(s, (x) => x == 34))
      assert(exists(s, (x) => x < 34))
      assert(!exists(s, (x) => x > 34))
      assert(!exists(s, (x) => x == 340))

    }
  }


  @Test def `map set`: Unit = {
    new TestSets {
      val s = union(union(union(s1, s2), s3), s34)
      val t = map(s, x => x * 2)
      assert(contains(t, 1 * 2))
      assert(contains(t, 2 * 2))
      assert(contains(t, 3 * 2))
      assert(contains(t, 34 * 2))
      assert(!contains(t, 55))
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
