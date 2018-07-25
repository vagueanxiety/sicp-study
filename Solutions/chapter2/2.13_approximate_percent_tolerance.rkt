;x, y two intervals
;xc, yc the centers of x and y
;**** xt, yt the percentage tolerances of x and y ****
;by the definition of percentage tolerances,
;xw, the width of x, = xt * xc, same for y
;x: (xc-xw, xc+xw) = (xc - xt * xc, xc + xt * xc)
;y: (yc-yw, yc+yw) = (yc - yt * yc, yc + yt * yc)
;Assumptions:
;1. all numbers are positive.
;2. tolerances are small
;
;from assumption1: 
;let z = x*y
;z = (xl * yl, xu * yu)
;z = ((xc - xt * xc) * (yc - yt * yc), (xc + xt * xc) * (yc + yt * yc)) 
;z = (xc*yc - xc*yt*yc - yc*xt*xc + xt*yt*xc*yc, xc*yc + xc*yt*yc + yc*xt*xc + xt*yt*xc*yc)
;
;from assumption2:
; xt*yt is very small = 0
; zw = (zu - zl)/2 = (xc*yc + xc*yt*yc + yc*xt*xc - (xc*yc - xc*yt*yc - yc*xt*xc) ) / 2
; zw =  xc*yt*yc + yc*xt*xc = yc*xc*(yt + xt) = yc*xc*(yt + xt)
; zc = (zu + zl)/2 = xc*yc
; **** zt = zw / zc ****
; **** zt = yc*xc*(yt + xt) / zc = yt + xt ****

; xt, yt => (xt + yt) = zt