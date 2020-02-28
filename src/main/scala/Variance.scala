package essential.variance

object Main {
  def main: Unit = {
    val covariance: MyCovariantList[Animal] =
      MyCovariantList[Cat](Cat("Whiskers"))

    val cat = Cat("Cherry")

    val catListContra = new CatList
    val animalListContra = new AnimalList
    def printer(list: MyContravariantList[Cat]): Unit = list.element(cat)
    printer(catListContra)
    printer(animalListContra)

    val catContra = new CatContra
    val animalContra = new AnimalContra
    catContra.print(cat)
    animalContra.print(cat)
  }
}

case class MyCovariantList[+A](element: A)

trait MyContravariantList[-A] {
  def element(value: A): Unit
}
class AnimalList extends MyContravariantList[Animal] {
  def element(value: Animal): Unit = ()
}
class CatList extends MyContravariantList[Cat] {
  def element(value: Cat): Unit = ()
}

abstract class MyContravariant[-A] {
  def print(value: A): Unit
}

class CatContra extends MyContravariant[Cat] {
  def print(value: Cat): Unit = Console.println(value.name)
}

class AnimalContra extends MyContravariant[Animal] {
  def print(value: Animal): Unit = Console.println(value.name)
}

sealed trait Animal {
  def name: String
}
final case class Cat(name: String) extends Animal
final case class Dog(name: String) extends Animal
