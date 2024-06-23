/*
*********************  ALGEBRAIC DATATYPES  ***************************************************************

- correctness of programs
--first: implementantion
--second: design
--> SAME FOR DATATYPES AND OPERATIONS
- we will introduce a language that allows us to talk about the "design" of a datatype and its operation

Nat type - natural numbers
How we build natural numbers?
Zero is a natural number, on the other hand,
we build a natural number as a SUCCESSOR of another natural number.
imagine these "constructions" as _boxes_.

- First box: "Zero" - we cannot open/ it has nothing inside, BUT labeled as "zero" and
will be used to represent this number
- Second type of box: "Succ" - always has something inside it
- Another box of type "Nat".
- If the inner box is the number n, then the outer box is n + 1
---> these are called BASE CONSTRUCTORS in formal language.
*/

/*
---- Operations:
- will be defined by "looking at the box type" / "opening the box".
- operators are defined wrt base constructors

isEmpty(Zero) = false
isEmpty(Succ(x)) = true

- Addition over natural numbers:
add: Nat x Nat -> Nat
add(Zero, m) = m       if the first box is Zero, then m
add(Succ(n), m) = Succ(add(n, m))
  if the first box is Succ, then we open it, we take the inner box, we add it (recursively) to m
  and place the result in the box
  We recursively open all boxes and build the representation for n + m
 */

/*
Practice:
greater: Nat x Nat -> Boolean
greater(Zero, m) = false
greater(Succ(n), Zero) = true
greater(Succ(n), Succ(m)) = greater(n, m)
- "Axioms" / "Laws" --> express how any implementation of add and greater should behave, irrespective of the PL that is used
 */

trait Nat // traits are very similar to interfaces
case object Zero extends Nat
case class Succ(x: Nat) extends Nat

/*
- Case classes are different from Java classes.
- Features:
- 1. Case classes are immutable (we cannot change them)
- 2. Members of case classes are immutable (e.g.: we cannot change x)
- 3. Comparison between objects build using case classes is done structurally (not referentially). Objects are identical if they are built in the same way
- 4. Case classes have a SINGLE CONSTRUCTOR
- 5. The "construction" of an object (opening the box) can be examined using the match instruction
 */

def isZero(n: Nat): Boolean = {
  n match {
    case Zero => false
    case Succ(_) => true
  }
}

def add(n: Nat, m: Nat): Nat = {
  n match {
    case Zero => m
    case Succ(np) => Succ(add(np, m))
  }
}

/*
- Note that the match expression contains patterns.
- They are evaluated in the order of their definition.
- Patterns can be very expensive, in order words,
        we can look in any detail we want at the construction of an object
 */

def largerThanThree(n: Nat): Boolean = {
  n match {
    case (Succ(Succ(Succ(_)))) => true
    case _ => false
  }
}

//println(largerThanThree(Succ(Succ(Succ(Succ(Zero))))))

/*
There is also a very neat way of looking "in multiple boxes" at the same time, using PAIRS.
Suppose we want to design a function that returns true IF
                  at least one of its parameters is larger than two
 */

def atLeastOneLargerThanTwo(n: Nat, m: Nat): Boolean = {
  (n, m) match {
    case (Succ(Succ(_)), _) => true
    case (_, Succ(Succ(_))) => true
    case _ => false
  }
}

/*
- Practice: define GREATER
 */

def greater(n: Nat, m: Nat): Boolean = {
  (n, m) match {
    case (Zero, _) => false
    case (_, Zero) => true
    case (Succ(np), Succ(mp)) => greater(np, mp)
  }
}

/*
- More details at another more useful datatype:

Void: List
Cons: Int x List -> List

- Similar to Nat, lists are constructed using two type of objects.
- Here, the second type of box contains two compartments:
    - the current value of the list, called the "HEAD"
    - the rest of the list, called the "TAIL"
 */

/*
- We represent the list 1,2,3 as
    Cons(1, Cons(2, Cons(3, Void)))

We can also use a datatype representation for elements contained in the list.
For simplicity, we opt for integers.
 */

/*
- We now define the following operations on lists:

isEmpty: List -> Boolean
size: List -> Int
append: List x List -> List
reverse: List x List -> List (two different ways)

and we implement them in Scala
 */
