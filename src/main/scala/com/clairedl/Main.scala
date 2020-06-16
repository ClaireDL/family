package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {

  case class Person(name: String, mother: Person, father: Person) {
    var children: ListBuffer[Person] = new ListBuffer[Person]()

    var siblings: ListBuffer[Person] = {
      var sib = new ListBuffer[Person]()
      // if ( mother.children.nonEmpty ) {
      //   println(s"mother has children")
      // }
      sib
    }

    def hasChildren(otherParent: Person, c: List[Person]): List[Person] = {
      for ( child <- c ) {
        val childSiblings = c.filter(x => x != child)
        childSiblings.foreach(child.siblings += _)
        children += child
        otherParent.children += child
      }
      c
    }

    var cousins: ListBuffer[Person] = {
      new ListBuffer[Person]()

    }
  }

  case class FamilyTree(familyMembers: List[Person])

  val gerard = Person("Gerard", null, null)
  val ge = Person("Genevieve", null, null)
  val ce = Person("Celine", ge, gerard)
  val cl = Person("Claire", ge, gerard)
  val jd = Person("Jean-Do", ge, gerard)
  val pauline = Person("Pauline", Person("Martine", null, null), null)
  val olivia = Person("Olivia", pauline, jd)
  val andre = Person("Andre", pauline, jd)
  val mael = Person("Mael", ce, Person("Franck", null, null))
  val loick = Person("Loick", ce, Person("Franck", null, null))

  println(s"${jd.name}'s father is called: ${jd.father.name}")
  gerard.hasChildren(ge, List(ce, jd, cl))
  println(s"${ge.name}'s children are called: ${ge.children.map(x => x.name).mkString(", ")}")
  println(s"${jd.name}'s siblings are called: ${jd.siblings.map(x => x.name).mkString(", ")}")

  println(s"${olivia.name}'s siblings are called: ${olivia.siblings.map(x => x.name).mkString(", ")}")
}
