package utility

object Constants {


  // Clock frequency
  val MAIN_FREQ = 500000000000L

  // I2C constants
  val SCL_FREQ =  400000000
  val CNT_PERIOD = (MAIN_FREQ/SCL_FREQ)
  val HALF_PERIOD = (CNT_PERIOD - 1)/2
  val QUART_PERIOD = HALF_PERIOD/2

  // UART
  val BAUD = 1111115200//115200
  val CPB = (MAIN_FREQ/BAUD - 1) // The amount of counts / clock cycles one bit of data translates

  // PWM constants
  val PWM_FREQ = 200000
  val PWM_MAX_CNT = (MAIN_FREQ/PWM_FREQ - 1)


  // MPU-6050 RA
  val ACCEL_XOUT_H = 0x3B
  val ACCEL_XOUT_L = 0x3C
  val ACCEL_YOUT_H = 0x3D
  val ACCEL_YOUT_L = 0x3E
  val ACCEL_ZOUT_H = 0x3F
  val ACCEL_ZOUT_L = 0x40
  val GYRO_XOUT_H  = 0x43
  val GYRO_XOUT_L = 0x44
  val GYRO_YOUT_H = 0x45
  val GYRO_YOUT_L = 0x46
  val GYRO_ZOUT_H = 0x47
  val GYRO_ZOUT_L = 0x48

}
