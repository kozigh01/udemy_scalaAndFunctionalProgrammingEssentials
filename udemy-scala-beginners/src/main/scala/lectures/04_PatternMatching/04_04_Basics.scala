package lectures.`04_PatternMatching`

import scala.util.Random

object PatternMatching:
  def basics =
    println("----- Pattern Matching Basics -----\n")

    /*
      1. cases are matched in orderj
      2. return type of match is the unification of all the types for all the cases
      3. pattern matching works really well with case classes since they include extracter features
    */
    
    // switch on steriods
    val random = Random(System.nanoTime())
    val x = random.nextInt(10)
    val description = x match
      case 1 => "the one"
      case 2 => "double or nothing"
      case 3 => "third times a charm"
      case _ => "something else"  // _ = WILDCARD

    println(s"x: $x, description: $description")

    // 1. decompose values
    case class Person(name: String, age: Int)
    val bob = Person("bob", 20)
    val greeting = bob match
      case Person(n, a) => s"Hello, I am $n and I am $a years old"
      case _ => "I don't know who I am"
    println(greeting)
    
    // can add "gaurds" for cases
    val random2 = Random(System.nanoTime())
    val bob2 = Person("bob2", random.nextInt(50))
    val greeting2 = bob2 match
      case Person(n, a) if a < 30 => s"Hello, I am $n and I am $a years old, I am a pup"
      case Person(n, a) => s"Hello, I am $n and I am $a years old, I am old"
      case _ => "I don't know who I am"
    println(greeting2)

    // pattern matching on sealed hierarchies
    sealed class Animal
    case class Dog(breed: String) extends Animal
    case class Parrot(greeting: String) extends Animal

    val animal: Animal = Dog("Terra Nova")
    animal match
      case Dog(b) => println(s"Matched a dog of the breed $b")
      case Parrot(g) => println(s"Matched a parrot who says '$g'")
    
  def exercises =
    println("----- Pattern Matching Basics - Exercises -----\n")


@main def patterMatchingBasics =
  println("---------- Pattern Matching ----------")
  
  // PatternMatching.basics
  PatternMatching.exercises
  
  println("\n---------- End Pattern Matching ----------")
