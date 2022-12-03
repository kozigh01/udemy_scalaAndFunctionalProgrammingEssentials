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
  def map[V >: T, U >: T](transformer: MyTransformer[V, U]): MyList3[U]

class MyList3Empty[T <: Nothing] extends MyList3[T]:
  def head(): Nothing = throw new NoSuchElementException
  def tail(): MyList3[Nothing] = throw new NoSuchElementException
  def isEmpty(): Boolean = true
  def add[U >: T](n: U): MyList3[U] = MyList3NonEmpty(n, this)
  override def printElements(): String = ""
  def map[V >: Nothing, U >: T](transformer: MyTransformer[V, U]): MyList3[U] = MyList3Empty()

class MyList3NonEmpty[+T](val headVal: T, val tailVal: MyList3[T]) extends MyList3[T]:
  def head(): T = headVal
  def tail(): MyList3[T] = tailVal
  def isEmpty(): Boolean = false
  def add[U >: T](n: U): MyList3[U] = MyList3NonEmpty[U](n, this)
  override def printElements(): String = 
    this.isEmpty() match
      case true => ""
      case false => s"${this.tail().printElements()} ${this.head()}" 
  def map[V >: T, U >: T](transformer: MyTransformer[V, U]): MyList3[U] =
    this.isEmpty() match
      case true => MyList3Empty()
      case false =>
        val newHead = transformer.convert(this.head()) 
        new MyList3NonEmpty(newHead, tail().map(transformer))
    

class Animal(val name: String):
  override def toString(): String = s"I am an Animal named '$name'"
class Dog(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Dog named '$name'"
class Cat(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Cat named '$name'"
class Bird(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Bird named '$name'"

@main def myList3() =
  val mylist = MyList3Empty()
  println(s"mylist: ${mylist.toString()}")

  val cat = Cat("boots")
  val mylist1 = mylist.add(cat)
  println(s"mylist: ${mylist1.toString()}")

  val dog = Dog("rusty")
  val MyList3 = mylist1.add(dog)
  println(s"mylist: ${MyList3.toString()}")
  
  val bird = Bird("tweety")
  val mylist3 = MyList3NonEmpty[String]("!", MyList3NonEmpty("Scala", MyList3NonEmpty("Hello", MyList3Empty())))
  println(s"mylist: ${mylist3.toString()}")
