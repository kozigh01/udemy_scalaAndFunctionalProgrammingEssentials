package exercises.c

import exercises.MyListNonEmpty

trait MyPredicate[-T]:
  def test(el: T): Boolean

// abstract class MyPredicateG[T] extends MyPredicate[T]
// class MyPredicateBird extends MyPredicateG[Bird]:
//   // override def test[U >: T](el: U): Boolean = 
//   //   val result = el.name == "Tweety"
//   //   result
//   // override def test(el: Animal): Boolean =
//   //   val result = el.name == "Tweety"
//   //   result  
//   def test(el: Animal): Boolean =
//     val result = el.name == "Tweety"
//     result  

trait MyTransformer[A, B]:
  def transform(el: A): B

abstract class MyList3[+T]:
  def head(): T
  def tail(): MyList3[T]
  def isEmpty(): Boolean
  def add[U >: T](el: U): MyList3[U]
  def printElements(): String
  override def toString(): String = s"[${printElements()}]"
  def map[V >: T, U](transformer: MyTransformer[V, U]): MyList3[U]
  def filter[V >: T](predicate: MyPredicateG[V]): MyList3[V]

class MyList3Empty extends MyList3[Nothing]:
  def head(): Nothing = throw new NoSuchElementException
  def tail(): MyList3[Nothing] = throw new NoSuchElementException
  def isEmpty(): Boolean = true
  def add[U >: Nothing](n: U): MyList3[U] = MyList3NonEmpty(n, this)
  override def printElements(): String = ""
  def map[V >: Nothing, U >: Nothing](transformer: MyTransformer[V, U]): MyList3[U] = MyList3Empty()
  def filter[T](predicate: MyPredicateG[T]): MyList3[T] = this

class MyList3NonEmpty[+T](val headVal: T, val tailVal: MyList3[T]) extends MyList3[T]:
  def head(): T = headVal
  def tail(): MyList3[T] = tailVal
  def isEmpty(): Boolean = false
  def add[U >: T](element: U): MyList3[U] = MyList3NonEmpty[U](element, this)
  override def printElements(): String = 
    this.isEmpty() match
      case true => ""
      case false => s"${this.tail().printElements()} ${this.head()}" 
  def map[V >: T, U](transformer: MyTransformer[V, U]): MyList3[U] =
    this.isEmpty() match
      case true => MyList3Empty()
      case false =>
        val newHead = transformer.transform(this.head()) 
        new MyList3NonEmpty(newHead, tail().map(transformer))
  // def filter[V >: T](predicate: MyPredicate[V]): MyList3[V] = 
    // this.isEmpty() match
    //   case true => MyList3Empty()
    //   case false =>
    //     if this.isEmpty() then
    //       MyList3Empty()
    //     else  
    //       val newHead = this.head()
    //       new MyList3NonEmpty(newHead, tail())
  def filter[V >: T](predicate: MyPredicateG[V]): MyList3[V] = 
    (this.isEmpty(), predicate.test(this.headVal)) match
      case (true, _) => MyList3Empty()
      case (isEmpty, false) =>
        val newHead = this.head()
        if isEmpty then MyList3Empty() else this
      case (false, true) =>
        val newHead = this.head()
        new MyList3NonEmpty(newHead, tail().filter(predicate))    

class Animal(val name: String):
  override def toString(): String = s"I am an Animal named '$name'"
class Dog(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Dog named '$name'"
class Cat(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Cat named '$name'"
class Bird(name: String) extends  Animal(name):
  override def toString(): String = s"I am a Bird named '$name'"

@main def myListCovariance() =
  val mylist: MyList3[Cat] = MyList3Empty()
  println(s"mylist: ${mylist.toString()}")

  val cat = Cat("boots")
  val mylist1 = mylist.add(cat)
  println(s"mylist: ${mylist1.toString()}")

  val dog = Dog("rusty")
  val mylist2 = mylist1.add(dog)
  
  val bird = Bird("tweety")
  val mylist3 = mylist2.add(bird)
  println(mylist3.head().getClass())
  println(s"mylist: ${mylist3.toString()}")

  val transformer = new MyTransformer[Animal, Bird]:
    override def transform(el: Animal): Bird = Bird(el.name)
  val mylist4 = mylist3.map(transformer)
  println(mylist4.getClass())
  println(mylist4.head().getClass())
  println(s"mylist: ${mylist4.toString()}")
     
  val myFilter5 = MyPredicateBird
  val mylist5 = mylist4.filter(myFilter5)
  println(s"mylist: ${mylist5.toString()}")
  
  val mylistSting = MyList3NonEmpty[String]("!", MyList3NonEmpty("Scala", MyList3NonEmpty("Hello", MyList3Empty())))
  println(s"mylist: ${mylist3.toString()}")
