s0.initCam(1)
src(s0).out()

src(s0)
.contrast(2)
.modulate(voronoi(7).modulate(osc(1,0.2)))
.modulate(osc(2,-0.1))
.out()

hush()

src(s0).contrast(4).brightness(0.3).out()
