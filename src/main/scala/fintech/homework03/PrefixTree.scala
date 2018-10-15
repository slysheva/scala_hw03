package fintech.homework03

// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait IPrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): IPrefixTree[K, V]
  def get: V
}

class PrefixTree[K, +V](val value: Option[V],val children: Map[K, PrefixTree[K, V]]) extends IPrefixTree[K, V] {
  override def equals(other: Any): Boolean = {
    other match {
      case that: PrefixTree[K, V] => that.children == children && that.value.getOrElse() == value.getOrElse()
      case _ => false
    }
  }

  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = {
    if (path.isEmpty)
       new PrefixTree[K, U](Some(value), children)
    else if (children contains path.head)
        new PrefixTree[K, U](this.value, children + (path.head -> children(path.head).put(path.tail, value)))
    else
      new PrefixTree[K, U](this.value, children + (path.head -> new PrefixTree[K, U](None, Map()).put(path.tail, value)))
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (path.isEmpty)
       this
    else if (children contains path.head)
       children(path.head).sub(path.tail)
    else
        new PrefixTree[K, V](None, Map())
  }

  override def get: V = value.get
}

