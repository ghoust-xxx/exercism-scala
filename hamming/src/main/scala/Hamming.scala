object Hamming {
  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] =
    if (dnaStrandOne.length != dnaStrandTwo.length) {
      None
    } else {
      Some(dnaStrandOne.toList.zip(dnaStrandTwo.toList).filter(x => x._1 != x._2).size)
    }
}
