# tcalc
[![Build Status](https://travis-ci.org/rdnetto/tcalc.svg?branch=master)](https://travis-ci.org/rdnetto/tcalc)

Tcalc is a command-line calculator for performing time-oriented computations.

## Installation
Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed, then run:

    stack install

## Syntax
### Literals
Tcalc literals are either *scalars* or *durations*. A *scalar* is a signed floating point number. A *duration* is (effectively) a scalar number of seconds. The syntax for a duration consists of one or more adjacent (not space separated)`[NUM][UNIT]` pairs, where `NUM` is an unsigned floating point number and `UNIT` is one of the following:

* `s` - seconds
* `m` - minutes
* `h` - hours
* `d` - days
* `w` - weeks

The *canonical form* of a duration is that which maximises the value of the largest units. i.e. `1w` instead of `7d`. Non-canonical forms of durations are accepted as input, but will be converted to canonical form on output. e.g.

```
70m120s
> 1h12m
```

### Operations
Tcalc supports standard arithmetic operators (addition, subtraction, multiplication, division, brackets).
Scalars and durations may be mixed freely where it makes sense to do so. e.g.

```
1w - 1d
> 6d
3d / 2h30m
> 28.8
2 * 40m
> 1h20m
1m * 1m
> Invalid operation (type mismatch): LitDuration (Duration 60.0) * LitDuration (Duration 60.0)
```
