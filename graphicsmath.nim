
proc mkString*[T](v : T, prefix, sep, postfix : string) : string =
  result = prefix
  var first = true
  for x in v:
    if not first:
      result.add(sep)
    result.add($x)
    first = false

  result.add(postfix)

type Vec[h : static[Natural], T] = distinct array[h, T]
type Mat[w, h : static[Natural], T] = distinct array[w, Vec[h, T]]
proc arr[h : static[Natural], T]( v : Vec[h,T] ) : array[h, T] = cast[array[h, T]](v)
proc arr[w, h : static[Natural], T]( m : Mat[w,h,T] ) = cast[array[w, Vec[h, T]]](m)

proc `$`[h : static[Natural], T](v : Vec[h,T]) : string =
  v.arr.mkString("vec" & $(h) & "(", ", ", ")")

proc `$`[w,h : static[Natural], T](v : Mat[w, h,T]) : string =
  let prefix = "Mat" & (if w == h: $w else: $w & "x" & $h) & "("
  v.arr.mkString(prefix, ", ", ")")

type Vec2f = Vec[2, float32]
type Vec3f = Vec[3, float32]
type Vec4f = Vec[4, float32]
type Mat2f = Mat[2, 2, float32]
type Mat3f = Mat[3, 3, float32]
type Mat4f = Mat[4, 4, float32]

proc vec2f(x,y : float32 ) : Vec2f = [x,y].Vec2f
proc vec3f(x,y,z : float32) : Vec3f = [x,y,z].Vec3f
proc vec4f(x,y,z,w : float32) : Vec4f = [x,y,z,w].Vec4f

proc mat2f(x,y : Vec2f) : Mat2f = [x,y].Mat2f
proc mat3f(x,y,z : Vec3f) : Mat3f = [x,y,z].Mat3f
proc mat4f(x,y,z,w : Vec4f) : Mat4f = [x,y,z,w].Mat4f

echo vec2f(1,2).Vec[2, float32]

echo mat2f(vec2f(1,2), vec2f(3,4))











