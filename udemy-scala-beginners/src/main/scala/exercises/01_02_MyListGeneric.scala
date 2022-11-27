package exercises

class MyListGeneric[T]:
  def head(): T
  def tail(): MyListGeneric[T]
  def isEmpty(): Boolean
  def add(el: T): MyListGeneric[T]
  def printElements(): String
  override def toString(): String = s"[${printElements()}]"

class MyListGenericEmpty[T] extends MyListGeneric[T]:
  def head(): T = throw new NoSuchElementException
  def tail(): MyListGeneric[T] = throw new NoSuchElementException
  def isEmpty(): Boolean = true
  def add[T](n: T): MyListGenericNonEmpty[T] = MyListGenericNonEmpty[T](n, this)
  override def printElements(): String = ""

class MyListGenericNonEmpty[T](val headVal: T = 0, val tailVal: MyListGeneric[T]  = null) extends MyListGeneric[T]:
  def head(): T = headVal
  def tail(): MyListGeneric[T] = tailVal
  def isEmpty(): Boolean = false
  def add[T](n: T): MyListGenericNonEmpty[T] = MyListGenericNonEmpty[T](n, this)
  override def printElements(): String = 
    this.isEmpty() match
      case true => ""
      case false => s"${this.tail().printElements()} ${this.head()}" 

@main def myListGeneric() =
  val mylist = MyListGenericEmpty[String]()
  println(s"mylist: ${mylist.toString()}")

  val mylist1 = mylist.add("11")
  println(s"mylist: ${mylist1.toString()}")

  val mylist2 = mylist1.add("22")
  println(s"mylist: ${mylist2.toString()}")
  
  val mylist3 = MyListGenericNonEmpty[String]("3", MyListGenericNonEmpty("2", MyListGenericNonEmpty("1", MyListGenericEmpty())))
  println(s"mylist: ${mylist3.toString()}")
