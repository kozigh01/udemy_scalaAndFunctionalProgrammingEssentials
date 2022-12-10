package lectures.`03_FP`

object HOFandCurriedFunctions extends App {
  
  // val superFunction: (Int, (String, (Int => Boolean)) => Int) => (Int => Int) = ???

  // Higher order function (HOF): takes a function as param or returns a function
  //    Example: map, flatMap, filter

  // function that applies a function n times over a value x
  //    nTimes(f, n, x)
  //    nTimes(f, 3, x) = f(f(f(x)))
  val nTimes: (Int => Int, Int, Int) => Int = (f, n, x) =>
    n match
      case n if n <= 0 => x
      case n => nTimes(f, n-1, f(x))
  println(s"nTimes(_ * 2, 3, 10): ${nTimes(_ * 2, 3, 10)}")
  println(s"nTimes(_ + 1, 7, 10): ${nTimes(_ + 1, 7, 10)}")
    
  // nTimesBetter
  val nTimesBetter: (f: Int => Int, n: Int) => (Int => Int) = (f,n) =>
    n match
      case n if n <= 0 => (x: Int) => x
      case n => (x: Int) => nTimesBetter(f, n-1)(f(x))
  println(s"nTimesBetter(_ * 2, 3)(10): ${nTimesBetter(_ * 2, 3)(10)}")
  val increment10By5 = nTimesBetter(_ + 10, 5)
  println(s"increment10(10): ${increment10By5(5)}")
      
}
