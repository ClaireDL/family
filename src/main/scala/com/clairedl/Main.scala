package com.clairedl.scala

object Main extends App {
  abstract class FamilyTree(members: List[Person])

  case class Person(name: String, gender: String) {
    def hasChildren(otherParent: Person, children: List[Person]): String = {
      s"$name has ${children.length} children with ${otherParent.name}."
    }

    def hasMother(mother: Person): String = s"$name's mother is ${mother.name}"
    def hasFather(father: Person): String = s"$name's father is ${father.name}"

    def hasSiblings: String = {

      }

    }
  }

  val gerard = Person("Gerard", "male")
  val genevieve = Person("Genevieve", "female")
  val ce = Person("Ce", "female")
  val cl = Person("Cl", "female")
  val jd = Person("JD", "male")
  println(gerard.hasChildren(genevieve, List(ce, cl, jd)))
}
