object CollatzConjecture {
  def steps(num: Int): Option[Int] = {
    if (num <= 0) {
      return None
    }
    return Some(iteration(num, 0))
  }

  def iteration(num: Int, cnt: Int): Int = {
    if (num == 1) {
      return cnt
    } else if (num % 2 == 0) {
      return iteration(num/2, cnt+1)
    } else {
      return iteration(3 * num + 1, cnt+1)
    }
  }
}
