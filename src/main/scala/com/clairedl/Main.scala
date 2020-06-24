package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {


  case class Person(name: String, isMale: Boolean) {
    var children: ListBuffer[Person] = new ListBuffer[Person]()

    var siblings: ListBuffer[Person] = new ListBuffer[Person]()

    private var _father: Person = null
    def father: Person = this._father

    private var _mother: Person = null
    def mother: Person = this._mother

    /**
    * Sets a person's children with a specific parent
    * Establishes other relationships such as the children's father/mother and siblings
    */
    def hasChildren(otherParent: Person, c: List[Person]): List[Person] = {
      for ( child <- c ) {
        val childSiblings = c.filter(x => x != child)
        childSiblings.foreach(child.siblings += _)
        children += child
        otherParent.children += child
        if ( isMale ) {
          child._father = Person(this.name, true)
          child._mother = otherParent
        }
        else {
          child._mother = Person(this.name, true)
          child._father = otherParent
        }
      }
      c
    }
  }

  case class FamilyTree(familyMembers: List[Person])

  val gerard = Person("Gerard", true)
  val ge = Person("Genevieve", false)
  val ce = Person("Celine", false)
  val cl = Person("Claire", false)
  val jd = Person("Jean-Do", true)

  gerard.hasChildren(ge, List(ce, jd, cl))

  val pauline = Person("Pauline", false)
  val olivia = Person("Olivia", false)
  val andre = Person("Andre", true)

  jd.hasChildren(pauline, List(olivia, andre))
  val mael = Person("Mael", true)
  val loick = Person("Loick", true)

  println(s"${jd.name}'s father is called: ${jd.father.name}")
  println(s"${ge.name}'s children are called: ${ge.children.map(x => x.name).mkString(", ")}")
  println(s"${jd.name}'s siblings are called: ${jd.siblings.map(x => x.name).mkString(", ")}")
  println(s"${olivia.name}'s siblings are called: ${olivia.siblings.map(x => x.name).mkString(", ")}")
}
