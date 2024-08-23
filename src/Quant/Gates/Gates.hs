module Gates.Gates(_h, _x, _z, _y, _p, _t, _s, _cnot, _toffoli, _fredkin, _swap) where

import Value.Operator
_h :: Qop Bin 1 1
_h = mkQop' [
      ((O:>NNil, O:>NNil), 1 :+ 0),
      ((O:>NNil, I:>NNil), 1 :+ 0),
      ((I:>NNil, O:>NNil), 1 :+ 0),
      ((I:>NNil, I:>NNil), (-1) :+ 0)
    ]

_x :: Qop Bin 1 1
_x = mkQop' [
      ((O:>NNil, I:>NNil), 1:+ 0),
      ((I:>NNil, O:>NNil), 1:+ 0)
    ]

_y :: Qop Bin 1 1
_y = mkQop' [
      ((O:>NNil, I:>NNil), (-1):+ 0),
      ((I:>NNil, O:>NNil), 1:+ 0)
    ]

_p :: Double -> Qop Bin 1 1
_p l = mkQop' [
      ((O:>NNil, O:>NNil), 1:+ 0),
      ((I:>NNil, I:>NNil), exp (0 :+ (pi*l)))
    ]

_z :: Qop Bin 1 1
_z = _p pi

_s :: Qop Bin 1 1
_s = _p (pi/2)

_t :: Qop Bin 1 1
_t = _p (pi/4)

_cnot :: Qop Bin 2 2
_cnot = mkQop' [
      ((O:>O:>NNil, O:>O:>NNil), 1:+ 0),
      ((O:>I:>NNil, O:>I:>NNil), 1:+ 0),
      ((I:>O:>NNil, I:>I:>NNil), 1:+ 0),
      ((I:>I:>NNil, I:>O:>NNil), 1:+ 0)
    ]

_toffoli :: Qop Bin 3 3
_toffoli = mkQop' [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1:+ 0),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1:+ 0),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1:+ 0),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1:+ 0),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1:+ 0),
      ((I:>O:>I:>NNil, I:>O:>I:>NNil), 1:+ 0),
      ((I:>I:>O:>NNil, I:>I:>I:>NNil), 1:+ 0),
      ((I:>I:>I:>NNil, I:>I:>O:>NNil), 1:+ 0)
    ]

_fredkin :: Qop Bin 3 3
_fredkin = mkQop' [
      ((O:>O:>O:>NNil, O:>O:>O:>NNil), 1:+ 0),
      ((O:>O:>I:>NNil, O:>O:>I:>NNil), 1:+ 0),
      ((O:>I:>O:>NNil, O:>I:>O:>NNil), 1:+ 0),
      ((O:>I:>I:>NNil, O:>I:>I:>NNil), 1:+ 0),
      ((I:>O:>O:>NNil, I:>O:>O:>NNil), 1:+ 0),
      ((I:>O:>I:>NNil, I:>I:>O:>NNil), 1:+ 0),
      ((I:>I:>O:>NNil, I:>O:>I:>NNil), 1:+ 0),
      ((I:>I:>I:>NNil, I:>I:>I:>NNil), 1:+ 0)
    ]

_swap :: Qop Bin 2 2
_swap = mkQop' [
      ((O:>O:>NNil, O:>O:>NNil), 1:+ 0),
      ((O:>I:>NNil, I:>O:>NNil), 1:+ 0),
      ((I:>O:>NNil, O:>I:>NNil), 1:+ 0),
      ((I:>I:>NNil, I:>I:>NNil), 1:+ 0)
    ]
