import FSets._

class FSetsTests extends munit.FunSuite {

  val empty: Int => Boolean = x => false

  test("Valid profile id:"+profileID){
    assert(profileID > 0)
  }

  test("Member (0p):"){
    assert(!member(1)(empty))
    assert(!member(10)(empty))
  }

  test("Singleton (0p):"){
    assert(member(1)(singleton(1)))
    assert(member(10)(singleton(10)))
    assert(!member(1)(singleton(2)))
  }

  test("Ins (0p):"){
    assert(member(1)(ins(1)(singleton(0))))
    assert(member(0)(ins(1)(singleton(0))))
    assert(!member(2)(ins(1)(singleton(0))))
  }

  test("From bounds (4p):"){
    assert(member(1)(fromBounds(1,3)))
    assert(member(2)(fromBounds(1,3)))
    assert(member(3)(fromBounds(1,3)))
    assert(!member(4)(fromBounds(1,3)))
  }

  test("Union (4p):"){
    assert(member(1)(union(singleton(1),singleton(2))))
    assert(member(2)(union(singleton(1),singleton(2))))
    assert(!member(0)(union(singleton(1),singleton(2))))
  }

  test("Complement (2p):"){
    assert(!member(1)(complement(singleton(1))))
    assert(member(0)(complement(singleton(1))))
  }

  test("sumSet (15p):"){
    assert(sumSet(0)(1,1)(singleton(1)) == 1)
    assert(sumSet(99)(1,1)(singleton(1)) == 100)
    assert(sumSet(99)(1,1)(singleton(0)) == 99)
    assert(sumSet(99)(2,1)(singleton(2)) == 99)
    assert(sumSet(0)(1,3)(union(singleton(1),union(singleton(2),singleton(3)))) == 6)
  }

  test("FoldLeftSet (15p):"){
    assert(foldLeftSet(0)(_ + _)(1,1)(singleton(1)) == 1)
    assert(foldLeftSet(99)(_ + _)(1,1)(singleton(1)) == 100)
    assert(foldLeftSet(99)(_ + _)(1,1)(singleton(0)) == 99)
    assert(foldLeftSet(99)(_ + _)(2,1)(singleton(2)) == 99)
    assert(foldLeftSet(0)(_ + _)(1,3)(union(singleton(1),union(singleton(2),singleton(3)))) == 6)
    assert(foldLeftSet(100)(_ - _)(1,3)(union(singleton(1),union(singleton(2),singleton(3)))) == 94)
  }
  test("FoldRightSet (15p):"){
    assert(foldRightSet(0)(_ + _)(1,1)(singleton(1)) == 1)
    assert(foldRightSet(99)(_ + _)(1,1)(singleton(1)) == 100)
    assert(foldRightSet(99)(_ + _)(1,1)(singleton(0)) == 99)
    assert(foldRightSet(99)(_ + _)(2,1)(singleton(2)) == 99)
    assert(foldRightSet(0)(_ + _)(1,3)(union(singleton(1),union(singleton(2),singleton(3)))) == 6)
    assert(foldRightSet(0)(_ - _)(1,3)(union(singleton(1),union(singleton(2),singleton(3)))) == 2)
  }
  test("Filter (4p):"){
    assert(!member(1)(filter(_ %2 == 0)(union(singleton(1),union(singleton(2),singleton(3))))))
    assert(member(2)(filter(_ %2 == 0)(union(singleton(1),union(singleton(2),singleton(3))))))
    assert(!member(3)(filter(_ %2 == 0)(union(singleton(1),union(singleton(2),singleton(3))))))
  }
  test("Partition (4p):"){
    val pair = partition(_%2 == 0)(union(singleton(1),union(singleton(2),singleton(3))))
    assert(!member(1)(pair._1))
    assert(member(2)(pair._1))
    assert(!member(3)(pair._1))
    assert(member(1)(pair._2))
    assert(!member(2)(pair._2))
    assert(member(3)(pair._2))

  }

  test("Forall (15p):"){
    assert(forall(_ => false)(0,100)(empty))
    assert(forall(_ % 2 == 0)(0,100)(filter(_ % 2 == 0)(fromBounds(0,100))))
    assert(!forall(_ % 2 == 0)(0,100)(union(singleton(1),filter(_ % 2 == 0)(fromBounds(0,100)))))
  }

  test("Exists (4p):"){
    assert(!exists(_ => true)(0,100)(empty))
    assert(!exists(_ % 2 == 0)(0,100)(filter(_ % 2 == 1)(fromBounds(0,100))))
    assert(exists(_ % 2 == 0)(0,100)(union(singleton(1),filter(_ % 2 == 0)(fromBounds(0,100)))))
  }

  test("Set of div by k (3p):"){
    assert(member(2)(setOfDivByK(2)))
    assert(member(22)(setOfDivByK(11)))
    assert(member(49)(setOfDivByK(7)))
    assert(!member(2)(setOfDivByK(7)))
  }

  test("MoreDivs (15p):"){
    assert(moreDivs(2)(0,100)(union(singleton(2),singleton(4)), singleton(2)))
    assert(moreDivs(2)(0,100)(setOfDivByK(2),setOfDivByK(4)))
    assert(!moreDivs(7)(0,100)(setOfDivByK(49),setOfDivByK(7)))

  }

  test("singleton") {
    val s = FSets.singleton(5)
    assert(FSets.member(5)(s))
    assert(!FSets.member(6)(s))
  }

  test("union") {
    val s1 = FSets.singleton(1)
    val s2 = FSets.singleton(2)
    val union = FSets.union(s1, s2)
    assert(FSets.member(1)(union))
    assert(FSets.member(2)(union))
    assert(!FSets.member(3)(union))
  }

  test("complement") {
    val s = FSets.singleton(5)
    val complement = FSets.complement(s)
    assert(!FSets.member(5)(complement))
    assert(FSets.member(6)(complement))
  }

  test("fromBounds") {
    val b = FSets.fromBounds(1, 10)
    assert(!FSets.member(0)(b))
    assert(FSets.member(5)(b))
    assert(FSets.member(10)(b))
    assert(!FSets.member(11)(b))
  }

  test("partition") {
    val b = FSets.fromBounds(1, 10)
    val partition = FSets.partition(_ % 2 == 0)(b)
    assert(FSets.member(2)(partition._1))
    assert(!FSets.member(3)(partition._1))
    assert(!FSets.member(2)(partition._2))
    assert(FSets.member(3)(partition._2))
  }

  test("filter") {
    val b = FSets.fromBounds(1, 10)
    val filter = FSets.filter(_ % 2 == 0)(b)
    assert(FSets.member(2)(filter))
    assert(!FSets.member(3)(filter))
  }

  test("setOfDivByK") {
    val divBy2 = FSets.setOfDivByK(2)
    assert(FSets.member(2)(divBy2))
    assert(!FSets.member(3)(divBy2))
  }

  // ----MyTestSet --------------------------------------------------------------------------

//  val a = FSets.singleton(5)
//  println(FSets.member(5)(a))
//  println(FSets.member(6)(a))
//
//  val s1 = FSets.singleton(1)
//  val s2 = FSets.singleton(2)
//  val u = FSets.union(s1, s2)
//  println(FSets.member(1)(u))
//  println(FSets.member(2)(u))
//  println(FSets.member(3)(u))
//
//  val c = FSets.complement(a)
//  println(FSets.member(5)(c))
//  println(FSets.member(6)(c))
//
//  val b = FSets.fromBounds(1, 10)
//  println(FSets.member(0)(b))
//  println(FSets.member(5)(b))
//  println(FSets.member(10)(b))
//  println(FSets.member(11)(b))
//
//  val p = FSets.partition(_ % 2 == 0)(b)
//  println(FSets.member(2)(p._1))
//  println(FSets.member(3)(p._1))
//  println(FSets.member(2)(p._2))
//  println(FSets.member(3)(p._2))
//
//  val f = FSets.filter(_ % 2 == 0)(b)
//  println(FSets.member(2)(f))
//  println(FSets.member(3)(f))
//
//  val divBy2 = FSets.setOfDivByK(2)
//  println(FSets.member(2)(divBy2))
//  println(FSets.member(3)(divBy2))

  // --------------------------------------------------------------------------------------





}
