# Math Modules

#### `math.pi: float`
PI as a float; `3.141592653589793`.

#### `math.nan: float`
NaN as a float value.

#### `math.inf: float`
Infinity as a float value.

#### `math.e: float`
Epsilon as a float value.

#### `math.abs(n: number): number`
Returns the absolute value of `n`.

#### `math.sqrt(n: number): number`
Returns the square root of `n`.

#### `math.exp(n: number): number`
Returns `math.e^n`, (the exponential function).

#### `math.exp2(n: number): number`
Returns `2^n`

#### `math.exp_m1(n: number): number`
Returns `e^n - 1` in a way that is accurate even if the number is close to zero.

#### `math.signum(n: number): number`
Returns a number that represents the sign of `n`.

- `1.0` if the number is positive, `+0.0` or `inf`
- `-1.0` if the number is negative, `-0.0` or `-inf`
- `NaN` if the number is `NaN`

#### `math.fract(n: number): number`
Returns the fractional part of `n`.

#### `math.sin(n: number): number`
Returns the sine of `n` (in radians).

#### `math.cos(n: number): number`
Returns the cosine of `n` (in radians).

#### `math.tan(n: number): number`
Returns the tangent of `n` (in radians).

#### `math.sinh(n: number): number`
Returns the hyperbolic sine of `n` (in radians).

#### `math.cosh(n: number): number`
Returns the hyperbolic cosine of `n` (in radians).

#### `math.tanh(n: number): number`
Returns the hyperbolic tangent of `n` (in radians).

#### `math.asin(n: number): number`
Returns the arcsine of `n` (in radians).

#### `math.acos(n: number): number`
Returns the arccosine of `n` (in radians).

#### `math.atan(n: number): number`
Returns the arctangent of `n` (in radians).

#### `math.asinh(n: number): number`
Returns the hyperbolic arcsine of `n` (in radians).

#### `math.acosh(n: number): number`
Returns the hyperbolic arccosine of `n` (in radians).

#### `math.atanh(n: number): number`
Returns the hyperbolic arctangent of `n` (in radians).

#### `math.deg(n: number): number`
Converts `n` radians to degrees.

#### `math.rad(n: number): number`
Converts `n` degrees to radians.

#### `math.random(): float`
Returns a thread local random float from `0` to `1`.
