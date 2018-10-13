package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: PrefixTree[Char, Int] = new PrefixTree[Char, Int](None, Map())

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)

    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }

  it should "work well with strings and different subtrees" in {
    val tree: PrefixTree[Char, Int] = new PrefixTree[Char, Int](None, Map())

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)

    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("aacde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("aa").sub("cde").get should be (13.0)
  }

  it should "work well with ints" in {
    val tree: PrefixTree[Int, Int] = new PrefixTree[Int, Int](None, Map())

    val with42: PrefixTree[Int, Int] = tree.put(Seq(1, 2, 3, 4), 42)

    with42.sub(Seq(1, 2)).sub(Seq(3, 4)).get should be (42)

    val withDouble: PrefixTree[Int, AnyVal] = with42.put(Seq(1, 2, 3, 4, 5), 13.0)
    withDouble.sub(Seq(1, 2)).sub(Seq(3, 4)).get should be (42)
    withDouble.sub(Seq(1, 2)).sub(Seq(3, 4, 5)).get should be (13.0)
  }

  it should "work well with any" in {
    val tree: PrefixTree[Any, Any] = new PrefixTree[Any, Any](None, Map())

    val with42: PrefixTree[Any, Any] = tree.put(Seq(1, 2.5, "a", 4), 42)

    with42.sub(Seq(1, 2.5)).sub(Seq("a", 4)).get should be (42)

    val withString: PrefixTree[Any, Any] = with42.put(Seq(1, 2.5, "a", 4, 5), "Hello")
    withString.sub(Seq(1, 2.5)).sub(Seq("a", 4)).get should be (42)
    withString.sub(Seq(1, 2.5)).sub(Seq("a", 4, 5)).get should be ("Hello")
  }

  it should "throw exception in case of incorrect path given" in {
    val tree: PrefixTree[Char, Int] = new PrefixTree[Char, Int](None, Map())

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    var exceptionCatched = false
    try {
      with42.sub("wrongPath")
    }
    catch  {
      case exception: NoSuchElementException  => exceptionCatched = true
    }
    if (!exceptionCatched)
      fail("There was no exception")
  }







}

