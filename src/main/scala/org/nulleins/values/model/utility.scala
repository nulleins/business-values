protected[phone] object utility {
  def digits(number: Long) = math.log10(number.toDouble).toInt + 1
  def normalize(number: String) = number.trim()
      .replaceAll("[^\\d]", "")
      .replaceFirst("^00", "")
      .toLong
}
