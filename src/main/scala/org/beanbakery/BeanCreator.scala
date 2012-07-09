package org.beanbakery

/**
 * Creates new empty beans based on a bean name.
 */
case class BeanCreator(name: Symbol, creator: () => AnyRef) extends ((Symbol) => Option[AnyRef]) {

  def apply(name: Symbol): Option[AnyRef] = {
    if (name == this.name) Some(creator())
    else None
  }

}