package utility

object Constants {
  val SCL_FREQ =  400000000
  val MAIN_FREQ = 500000000000L
  val PWM_FREQ = 200000
  val PWM_MAX = (MAIN_FREQ/PWM_FREQ - 1)
}
