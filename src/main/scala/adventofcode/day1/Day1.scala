package adventofcode.day1

object Day1 {

  /**
    * Sum valid elements of <code>list</code>.
    * An element <code>list[i]</code> of <code>list</code> is valide
    * iff <code>list[i]</code> equals <code>ref[i]</code>.
    *
    * @param list candidates to contribute to the sum
    * @param ref  references to compare to candidates
    * @param sum  sum of valid processed elements of <code>list</code>
    * @return Total sum of valid elements of <code>list</code>
    */
  private def sumValidElements(list: List[Char], ref: List[Char], sum: Int): Int =
    if (list.isEmpty)
      sum
    else if (list.head == ref.head)
      sumValidElements(list.tail, ref.tail, sum + list.head.asDigit)
    else
      sumValidElements(list.tail, ref.tail, sum)

  /**
    * A Char of <code>inputRaw</code> "inputRaw[i]" (i != 0) is valid (and must contribute to the final sum)
    * iff "inputRaw[i] = inputRaw[i-1]" (the first element "inputRaw[0]" is valid iff equals the last element).
    *
    * @param inputRaw input represented by a String
    * @return Total sum of valid elements of <code>inputRaw</code>
    */
  def solveCaptcha1(inputRaw: String): Int = {
    val input: List[Char] = inputRaw.toList
    val firstChar = input.head.asDigit
    val lastChar = input.last.asDigit

    // case of the first element
    val init = if (firstChar == lastChar) lastChar else 0

    sumValidElements(input.tail, input, init)
  }

  /**
    * Given n the inputRaw size (even).
    * A Char of <code>inputRaw</code> "inputRaw[i]" is valid (and must contribute to the final sum)
    * iff "inputRaw[i] = inputRaw[n/2 + i + 1 mod n]".
    *
    * @param inputRaw input represented by a String
    * @return Total sum of valid elements of <code>inputRaw</code>
    */
  def solveCaptcha2(inputRaw: String): Int = {
    val input: List[Char] = inputRaw.toList
    val (beginning, ending) = input.splitAt(input.size / 2);
    2 * sumValidElements(beginning, ending, 0)
  }

}
