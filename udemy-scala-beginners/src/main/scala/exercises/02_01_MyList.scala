package exercises.`02_01`

import exercises.MyListNonEmpty
import scala.annotation.tailrec

object MyList_02_01 extends App:

  abstract class MyList[+T]:
    def head(): T
    def tail(): MyList[T]
    def isEmpty(): Boolean
    def add[U >: T](el: U): MyList[U]
    def printElements(): String
    override def toString(): String = s"[${printElements()}]"
    def map[U](transformer: T => U): MyList[U]
    def flatMap[U](transformer: T => MyList[U]): MyList[U]
    def filter(predicate: T => Boolean): MyList[T]
    def ++[B >: T](list: MyList[B]): MyList[B]
    def foreach(func: T => Unit): Unit
    def sort(func: (T, T) => Int): MyList[T]
    def zipWith[U >: T, S](otherList: MyList[U], func: (U, U) => S): MyList[S]

  case class MyListEmpty() extends MyList[Nothing]:
    def head(): Nothing = throw new NoSuchElementException
    def tail(): MyList[Nothing] = throw new NoSuchElementException
    def isEmpty(): Boolean = true
    def add[U >: Nothing](n: U): MyList[U] = MyListNonEmpty(n, this)
    
    def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
    override def printElements(): String = ""
    def map[U >: Nothing](transformer: Nothing => U): MyList[U] = MyListEmpty()
    def flatMap[U](transformer: Nothing => MyList[U]): MyList[U] = MyListEmpty()
    def filter(prediCat5e: Nothing => Boolean): MyList[Nothing] = MyListEmpty()
    def foreach(func: Nothing => Unit): Unit = ()
    def sort(func: (Nothing, Nothing) => Int): MyList[Nothing] = MyListEmpty()
    def zipWith[U >: Nothing, S](otherList: MyList[U], func: (U, U) => S): MyList[S] = MyListEmpty()

  case class MyListNonEmpty[+T](val headVal: T, val tailVal: MyList[T]) extends MyList[T]:
    def head(): T = headVal
    def tail(): MyList[T] = tailVal
    def isEmpty(): Boolean = false
    def add[U >: T](element: U): MyList[U] = MyListNonEmpty[U](element, this)  
    def ++[B >: T](list: MyList[B]): MyList[B] =
      isEmpty() match
        case true => list
        case false =>
          MyListNonEmpty(head(), tail() ++ list)    
    override def printElements(): String = 
      this.isEmpty() match
        case true => ""
        case false => s"${this.tail().printElements()} ${this.head()}" 
    def map[U](transformer: T => U): MyList[U] =
      this.isEmpty() match
        case true => MyListEmpty()
        case false =>
          val newHead = transformer(this.head()) 
          MyListNonEmpty(newHead, tail().map(transformer))
    def flatMap[U](transformer: T => MyList[U]): MyList[U] =
      this.isEmpty() match
        case true => MyListEmpty()
        case false =>
          transformer(head()) ++ tail().flatMap(transformer)
    def filter(predicate: T => Boolean): MyList[T] = 
      (this.isEmpty(), predicate(this.headVal)) match
        case (true, _) => MyListEmpty()
        case (false, test) =>
          if test then
            new MyListNonEmpty(this.head(), tail().filter(predicate))  
          else
            tail().filter(predicate)
    def foreach(func: T => Unit): Unit = 
      func(head())
      tail().foreach(func)
    def sort(func: (T, T) => Int): MyList[T] = ???
    def zipWith[U >: T, S](otherList: MyList[U], func: (U, U) => S): MyList[S] = 
      MyListNonEmpty(func(head(), otherList.head()), tail().zipWith(otherList.tail(), func))

  class Animal(val name: String):
    override def toString(): String = s"I am an Animal named '$name'"
  class Dog(name: String) extends  Animal(name):
    override def toString(): String = s"I am a Dog named '$name'"
  class Cat(name: String) extends  Animal(name):
    override def toString(): String = s"I am a Cat named '$name'"
  class Bird(name: String) extends  Animal(name):
    override def toString(): String = s"I am a Bird named '$name'"


  def main() =
    // val mylist: MyList[Cat] = MyListEmpty()
    // println(s"mylist: ${mylist.toString()}")

    // val cat5 = Cat("boots")
    // val mylist1 = mylist.add(cat5)
    // println(s"mylist: ${mylist1.toString()}")

    // val dog5 = Dog("rusty")
    // val bird5 = Bird("tweety")
    // val mylist2 = mylist1.add(dog5)
    // val mylist3 = mylist2.add(bird5)
    // println(mylist3.head().getClass())
    // println(s"mylist: ${mylist3.toString()}")

    // val transformer: Animal => Bird = (el: Animal) => Bird(el.name)
    // val MyList = mylist3.map(transformer)
    // println(MyList.getClass())
    // println(MyList.head().getClass())
    // println(s"mylist transform: ${MyList.toString()}")
      
    // val myFilter5: Animal => Boolean = (el: Animal) => el.name.contains("tweety")
    // val mylist5 = MyList.filter(myFilter5)
    // println(s"mylist filtered: ${mylist5.toString()}")

    // val tranformer2: Animal => MyList[Animal] = (el: Animal) => {
    //   val mylist: MyList[Animal] = MyListEmpty()
    //   mylist.add(el).add(el).add(el)
    // }
    // val mylist6 = mylist3.flatMap(tranformer2)
    // println(s"mylist flatMap: ${mylist6.toString()}")
    
    // val myListString = MyListNonEmpty[String]("!", MyListNonEmpty("Scala", MyListNonEmpty("Hello", MyListEmpty())))
    // println(s"mylist: ${myListString.toString()}")

    // val myListStringClone = MyListNonEmpty[String]("!", MyListNonEmpty("Scala", MyListNonEmpty("Hello", MyListEmpty())))
    // println(s"myListString = myListStringClone: ${myListString == myListStringClone}")

    MyListEmpty()
      .add(Cat("boots")).add(Dog("rusty")).add(Bird("tweety"))
      .foreach(animal => println(s"foreach: $animal"))
    
    val list1_1 = MyListEmpty().add(Cat("boots")).add(Dog("rusty")).add(Bird("tweety"))
    val list1_2 = MyListEmpty().add(Cat("whiskers")).add(Dog("bandito")).add(Bird("big"))
    val list1Zip = list1_1.zipWith(list1_2, (l1, l2) => s"$l1 likes $l2")
    println(list1Zip)
    println(list1Zip.foreach(el => println(el)))

    
  main()