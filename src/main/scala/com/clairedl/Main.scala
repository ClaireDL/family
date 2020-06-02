package com.clairedl.scala

object Main extends App {
  abstract class FamilyTree(members: List[Person])

  case class Person(name: String) {
    def parents(mother: Person, father: Person): String = {
      s"${mother.name} and ${father.name} are $name's parents."
    }
    var siblings: List[Person] = List()

    def hasChildren(children: List[Person]): Unit = {
      for (child <- children) {
        val childSiblings = children.filter(x => x != child)
        child.siblings = childSiblings
      }
    }
  }


  val gerard = Person("Gerard")
  val genevieve = Person("Genevieve")
  val ce = Person("ce")
  val cl = Person("cl")
  val jd = Person("jd")
  println(gerard.hasChildren(List(ce, cl, jd)))
  println(ce.siblings)
  println(gerard.siblings)
}
