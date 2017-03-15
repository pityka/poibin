/** Density and distribution function of the Poisson Binomial distribution
  *
  * This is a translation of the original work of Yili Hong at
  * https://cran.r-project.org/web/packages/poibin/index.html
  *
  * License: GPL-2
  */
package object poibin {
  def fft(nvec: Array[Int], pp: Array[Double]): Array[Double] = {

    val npp: Int = pp.size
    val wts: Array[Int] = Array.fill[Int](pp.size)(1)
    val n: Int = wts.sum
    val avec: Array[Double] = Array.ofDim[Double](n + 1)
    val bvec: Array[Double] = Array.ofDim[Double](n + 1)
    val nn = nvec.size

    var i, j, k, kk, m, wtsj = 0
    var a2, b2, c1, c2 = 0d
    var tmp1, tmp2, ax, bx, pj = 0d
    var tt, delta, tres = 0d

    m = n + 1
    val PI = math.Pi
    delta = 2 * PI / m;

    avec(0) = 1.0;
    bvec(0) = 0.0;

    i = 1
    while (i <= math.ceil(n / 2) + 1) {
      var c1 = 0.00;
      var c2 = 0.00;
      var tt = i * delta;
      j = 0
      while (j < npp) {
        pj = pp(j)
        wtsj = wts(j)

        ax = 1 - pj + pj * math.cos(tt)
        bx = pj * math.sin(tt)
        tmp1 = math.sqrt(ax * ax + bx * bx)
        tmp2 = math.atan2(bx, ax)
        c1 += wtsj * math.log(tmp1)
        c2 += wtsj * tmp2
        j += 1
      }
      a2 = math.exp(c1) * math.cos(c2)
      b2 = math.exp(c1) * math.sin(c2)
      avec(i) = a2
      bvec(i) = b2
      avec(m - i) = a2
      bvec(m - i) = -1 * b2
      i += 1
    }

    val fft = new org.jtransforms.fft.DoubleFFT_1D(avec.size)
    val ab = Array.ofDim[Double](avec.size * 2)
    i = 0
    while (i < avec.size) {
      ab(2 * i) = avec(i)
      ab(2 * i + 1) = bvec(i)
      i += 1
    }
    fft.complexForward(ab)

    i = 0
    while (i < avec.size) {
      avec(i) = ab(2 * i)
      i += 1
    }
    avec
  }

  def cdf(nvec: Array[Int], pp: Array[Double]): Array[Double] = {
    val avec = fft(nvec, pp)
    val wts: Array[Int] = Array.fill[Int](pp.size)(1)
    val n: Int = wts.sum
    val m = n + 1
    var tres = 0d
    var kk = 0
    val nn = nvec.size

    avec(0) /= m
    var i = 1
    while (i < n) {
      tres = avec(i) / m
      avec(i) = tres + avec(i - 1)
      i += 1
    }
    val res = Array.ofDim[Double](nvec.size)
    var k = 0
    while (k < nn) {
      kk = nvec(k)
      if (kk < 0) {
        res(k) = 0d
      } else {
        tres = avec(kk)
        res(k) = tres
        if (res(k) < 0d) {
          res(k) = 0d
        }
        if (res(k) > 1d) {
          res(k) = 1d
        }
        if (kk >= n) {
          res(k) = 1d
        }
      }
      k += 1
    }

    res
  }

  def pdf(nvec: Array[Int], pp: Array[Double]): Array[Double] = {
    val avec = fft(nvec, pp)
    val wts: Array[Int] = Array.fill[Int](pp.size)(1)
    val n: Int = wts.sum
    val m = n + 1
    var tres = 0d
    var kk = 0
    val nn = nvec.size
    val res = Array.ofDim[Double](nvec.size)
    var k = 0
    while (k < nn) {
      kk = nvec(k)
      if (kk < 0 || kk > n) {
        res(k) = 0
      } else {
        res(k) = avec(kk) / m
        if (res(k) < 0) {
          res(k) = 0
        }
      }
      k += 1
    }

    res
  }
}
