d::Int->Double
d 1 = 1.0
d k = d (k-1) * sqrt (1 / (2*(1+cos(pi/3/(2^^(k-1))))))

s::Int->Double
s 1 = 2*d 1
s k = (2^(k)) * (d k) --s (k-1) - d (k-1) + 2 * (d k)

phi = (1.0+sqrt(5))/2.0