// osc(30,0.2,()=>time)
// src(o0)
osc(30,0.2,2)
.brightness(-1)
.saturate(-2.5)
// .saturate(()=>-time)
.rotate(Math.PI/180*90,0.01) // converting radians to angle
.pixelate(12,8)
// .kaleid([7,6,5,4,3])
.color(49/256, 159/256, 194/256)
// .rotate(Math.PI/180*90,-0.05) // converting radians to angle
// .kaleid([3,4,5,6,7])
.out()


speed = 0.05

osc(20,0,2)
// src(o0)
.rotate(Math.PI/180*90)
.pixelate(7,3)
.modulate(voronoi(10,0.2).pixelate(8,12).modulate(osc(3,2))).rotate(0,-0.6)
.kaleid([2,4].smooth().fast(0.5))
.modulate(voronoi(5,0.1).pixelate(12,8).modulate(osc(6,2))).rotate(0,0.6)
.rotate(Math.PI/180*90,-0.05) // converting radians to angle
// .modulate(osc(40,()=>(time/3)*20,0))
.contrast(-5)
.color(7/256, 194/256, 1/256)
.modulate(voronoi(20,0.1))
.brightness(-0.5)
.saturate(()=>Math.sin(time))
.out()


s0.initCam(1)
src(s0).out()

systemPreferences.askForMediaAccess('camera')

systemPreferences.askForMediaAccess('microphone')



// figure out how this works in js
let colour = (a,b,c) =>
   color(
    a/256,
    b/256,
    c/256
)


hush()

xres = window.innerWidth

yres = window.innerHeight

res = yres/xres

hush = (o) => {
  if (o == undefined) {
    solid().out(o0)
    solid().out(o1)
    solid().out(o2)
    solid().out(o3)
    render(o0)
    }
  solid().out(o)
  }



// osc(20,0,2)
src(s0)
.contrast(2)
// .modulate(voronoi(10,0.2).modulate(osc(3,2)))
// .kaleid(4)
// .saturate(()=>Math.sin(time))
.out()
