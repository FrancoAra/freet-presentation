package models.store

trait StoreDSL[T] {

  sealed trait StoreVerb[A]

  case class Put(key: String, value: T) extends StoreVerb[T]

  case class Get(key: String) extends StoreVerb[Option[T]]

  case class Delete(key: String) extends StoreVerb[Option[T]]
}
