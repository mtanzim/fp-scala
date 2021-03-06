package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {

  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
      assertEquals(9, weight(t2))
      assertEquals(2, weight(Leaf('a', 2)))

    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a', 'b'), chars(t1))
      assertEquals(List('a', 'b', 'd'), chars(t2))
      assertEquals(List('a'), chars(Leaf('a', 2)))

    }

  @Test def `test times`: Unit =
    new TestTrees {
      assertEquals(('b', 1) :: ('a', 1) :: Nil, times(List('a', 'b')))
      // TODO: is this deterministic?
      assertEquals(('b', 2) :: ('c', 1) :: ('a', 2) :: Nil, times(List('a', 'b', 'a', 'c', 'b')))
      assertEquals(('b', 2) :: Nil, times(List('b', 'b')))
      assertEquals(List(), times(List()))
      assertEquals(List(('c', 1)), times(List('c')))
    }

  @Test def `test codeTable`: Unit =
    new TestTrees {
      assertEquals(List(('a', List(0)), ('b', List(1))), convert(t1))
      assertEquals(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))), convert(t2))
    }

  @Test def `test codeBits`: Unit =
    new TestTrees {
      val ct1 = convert(t1)
      assertEquals(List(0), codeBits(ct1)('a'))
      assertEquals(List(1), codeBits(ct1)('b'))

      val ct2 = convert(t2)
      assertEquals(List(0,0), codeBits(ct2)('a'))
      assertEquals(List(0,1), codeBits(ct2)('b'))
      assertEquals(List(1), codeBits(ct2)('d'))

    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  //  @Test def `make code tree`: Unit =
  //    assertEquals(Leaf('c', 32), createCodeTree(string2Chars("Hello World")))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit = new TestTrees {
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
    assertEquals(List(Leaf('x', 3), Leaf('t', 23), Leaf('e', 45), Leaf('m', 99)), makeOrderedLeafList(List(('e', 45), ('t', 23), ('x', 3), ('m', 99))))
  }

  @Test def `test Tree singleton`: Unit = new TestTrees {
    assertEquals(true, singleton(List(t1)))
    assertEquals(false, singleton(t1 :: List(t1)))

  }


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist))
  }


  @Test def `decode test`: Unit =
    assertEquals(string2Chars("huffmanestcool"), decodedSecret)


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("abbabababababababbbbbababab".toList, decode(t1, encode(t1)("abbabababababababbbbbababab".toList)))
    }

  @Test def `decode and quick encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("abbabababababababbbbbababab".toList, decode(t1, quickEncode(t1)("abbabababababababbbbbababab".toList)))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
