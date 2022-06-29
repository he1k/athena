package utility

object Constants {
  val SCL_FREQ = 400000
  val MAIN_FREQ = 100000000
  val PWM_FREQ = 200000
  val PWM_MAX = (MAIN_FREQ/PWM_FREQ - 1)
}
