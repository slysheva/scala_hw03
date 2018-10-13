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

class PrefixTree[K, +V](val value: Option[V], children: Map[K, PrefixTree[K, V]]) extends IPrefixTree[K, V] {

  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = {
    if (path.isEmpty)
      return new PrefixTree[K, U](Some(value), children)
    var subTree: PrefixTree[K, U]  = this
    if (children contains path.head)
      subTree = children(path.head)
    new PrefixTree[K, U](this.value, children + (path.head -> subTree.put(path.tail, value)))
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (path.isEmpty)
      return this
    if (children contains path.head)
      return children(path.head).sub(path.tail)
    throw new NoSuchElementException
  }

  override def get: V = value.get
}

