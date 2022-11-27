package exercises.c

import exercises.MyListNonEmpty

trait MyPredicate[T]:
  def passesTest(el: T): Boolean

trait MyTransformer[A, B]:
  def convert(el: A): B

abstract class MyList3[+T]:
  def head(): T
  def tail(): MyList3[T]
  def isEmpty(): Boolean
  def add[U >: T](el: U): MyList3[U]
  def printElements(): String
  override def toString(): String = s"[${printElements()}]"
  def map[U](transformer: MyTransformer[T, U]): MyList3[U]

class MyList3Empty[Nothing] extends MyList3[Nothing]:
  def head(): Nothing = throw new NoSuchElementException
  def tail(): MyList3[Nothing] = throw new NoSuchElementException
  def isEmpty(): Boolean = true
  def add[T >: Nothing](n: T): MyList3[T] = MyList3NonEmpty(n, this)
  override def printElements(): String = ""
  def map[Nothing, U](transformer: MyTransformer[Nothing, U]): MyList3[U] = MyList3Empty()

class MyList3NonEmpty[+T](val headVal: T, val tailVal: MyList3[T]) extends MyList3[T]:
  def head(): T = headVal
  def tail(): MyList3[T] = tailVal
  def isEmpty(): Boolean = false
  def add[U >: T](n: U): MyList3[U] = MyList3NonEmpty[U](n, this)
  override def printElements(): String = 
    this.isEmpty() match
      case true => ""
      case false => s"${this.tail().printElements()} ${this.head()}" 
  def map[V <: T, U](transformer: MyTransformer[V, U]): MyList3[U] =
    this.isEmpty() match
      case true => MyList3Empty()
      case false =>
        val newHead = transformer.convert(this.head()) 
        new MyList3NonEmpty(newHead, tail().map(transformer))
    

@main def myList3() =
  val mylist = MyList3Empty()
  println(s"mylist: ${mylist.toString()}")

  val mylist1 = mylist.add("111")
  println(s"mylist: ${mylist1.toString()}")

  val MyList3 = mylist1.add("222")
  println(s"mylist: ${MyList3.toString()}")
  
  val mylist3 = MyList3NonEmpty[String]("!", MyList3NonEmpty("Scala", MyList3NonEmpty("Hello", MyList3Empty())))
  println(s"mylist: ${mylist3.toString()}")
