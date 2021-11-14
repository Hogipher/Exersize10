@main def main: Unit =
  object ex10 extends org.scalatest.FunSuite {
    type FSet[A] = A => Boolean

    def contains[A](elem: A, set: FSet[A]): Boolean = set(elem)

    def empty[A]: FSet[A] =
      x => false
    def insert[A](elem: A, set: FSet[A]): FSet[A] = x => (x == elem) || (set(x))
    def delete[A](elem: A, set: FSet[A]): FSet[A] =
      x => set(x) && x != elem
    def union[A](set1: FSet[A], set2: FSet[A]): FSet[A] =
      x => (set1(x) || set2(x))
    def filter[A](pred: A => Boolean, set: FSet[A]): FSet[A] =
      x => pred(x) && set(x)
    def preimage[A,B](f: A => B, set: FSet[B]): FSet[A] =
      x => set(f(x))

    // tests

    test("empty") {
      assertResult(false)(contains(5, empty))
    }
    test("insert") {
      val set = insert(5, insert(3, empty))
      assertResult(true)(contains(5, set))
      assertResult(true)(contains(3, set))
      assertResult(false)(contains(1, set))
    }
    test("delete") {
      val set = delete(3, insert(5, insert(3, empty)))
      assertResult(true)(contains(5, set))
      assertResult(false)(contains(3, set))
      assertResult(false)(contains(1, set))
    }
    test("union") {
      val set = union(insert(5, insert(3,empty)), insert(6, insert(3,empty)))
      assertResult(true)(contains(5, set))
      assertResult(true)(contains(3, set))
      assertResult(false)(contains(1, set))
    }
    test("filter") {
      def pred(x: Int): Boolean = x == 2 || x == 5
      val set = filter(pred, insert(2,insert(3,empty)))
      assertResult(false)(contains(1, set))
      assertResult(false)(contains(3, set))
      assertResult(true)(contains(2, set))
    }
    test("preimage") {
      def f(x: String): Int = x.length
      val set = preimage(f, insert(6,insert(4,empty)))
      assertResult(true)(contains("String", set))
      assertResult(true)(contains("Bean", set))
      assertResult(false)(contains("Mr", set))
    }

    def main(args: Array[String]): Unit = {
      println(userName);
      execute()
    }
  }
