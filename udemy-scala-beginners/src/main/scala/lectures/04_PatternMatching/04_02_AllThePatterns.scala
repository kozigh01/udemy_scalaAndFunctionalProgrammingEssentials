package lectures.`04_PatternMatching`

object AllThePatterns:
  def part1 =    
    println("----- Pattern Matching - All The Patterns -----\n")

    // 1. constants
    val x: Any = "Scala"
    val contants = x match
      case 1 => "a number"
      case "Scala" => "THE Scala"
      case true => "The Truth"
      case AllThePatterns => "A singleton object"
      case _ => "I don't know"
    println(s"constants: $contants")

    // 2. match anything
    val matchAnything = x match
      case _ => "I'm matching anything"
    println(s"matchAnything: $matchAnything")

    // 2.2 match a variable
    val matchAnything2 = x match
      case something => s"I'm matching anything: $something"
    println(s"matchAnything2: $matchAnything2")   

    // 3. tuples
    val tuple1 = 1 -> 2
    val match1 = tuple1 match
      case (1, 1) => "tuple (1, 1)"
      case (something, 2) => s"tuple (something, 2) - something = $something"
    println(s"match1: $match1")

    // 3a. nested tuples
    val tuple2 = (1, (2, 3))
    val match2 = tuple2 match
      case (_, (2, v)) => s"I found v = $v"

    // 4. case classes - costructor pattern (can be nested)
    // import exercises.`02_01`.MyList_02_01.{MyList, MyListEmpty, MyListNonEmpty}
    // val list1 = MyListNonEmpty(1, MyListNonEmpty(2, MyListEmpty()))
    // val match3 = list1 match
    //   case MyListEmpty => "I'm empty"
    //   case MyListNonEmpty(h, t) => s"Not empty: head = $h, tail = $t"
    //   case MyListNonEmpty(h, t(subhead, subtail)) => s"Not empty"
    // println(s"match3: $match3")

    // 5. list patterns
    val list1 = List(1,2,3,42)
    
    
    

    


@main def AllThePatternsMain =
  println("---------- All THe Patterns ----------")
  
  AllThePatterns.part1
  
  println("\n---------- All THe Patterns ----------")