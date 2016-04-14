
proc mkString*[T](v : T, prefix, sep, postfix : string) : string =
  result = prefix
  var first = true
  for x in v:
    if not first:
      result.add(sep)
    result.add($x)
    first = false

  result.add(postfix)

type
  Vec*[h : static[int]; T] = object
    arr* : array[h, T]
  Mat*[w,h: static[int]; T] = object
    arr* : array[w, Vec[h, T]]

proc `$`*[h : static[int], T](v : Vec[h,T]) : string =
  v.arr.mkString("vec" & $(h) & "(", ", ", ")")

proc `$`*[w,h : static[int], T](v : Mat[w, h,T]) : string =
  let prefix = "Mat" & (if w == h: $w else: $w & "x" & $h) & "("
  v.arr.mkString(prefix, ", ", ")")

proc dot*[h : static[int], T](v1, v2 : Vec[h,T]) : T =
  for i in 0 .. < h:
    result += v1.arr[i] * v2.arr[i]

template operatorVec( op : untyped ) =
  proc op*[h : static[int], T](v1, v2 : Vec[h,T]) : Vec[h,T] =
    for i in 0 .. < h:
      result.arr[i] = op(v1.arr[i], v2.arr[i])

operatorVec( `*` )
operatorVec( `/` )
operatorVec( `+` )
operatorVec( `-` )

template assignmentOperatorVec( op : untyped ) =
  proc op*[h : static[int], T](v1 : var Vec[h,T]; v2 : Vec[h,T]) =
    for i in 0 .. < h:
      op(v1.arr[i], v2.arr[i])

assignmentOperatorVec( `*=` )
assignmentOperatorVec( `/=` )
assignmentOperatorVec( `+=` )
assignmentOperatorVec( `-=` )
    
proc `-`*[h : static[int], T](v : Vec[h,T]) : Vec[h,T] =
  for i in 0 .. < h:
    result.arr[i] = -v.arr[i]

template operatorMat( op : untyped, interal_op ) =
  proc op*[w,h : static[int], T](m1,m2 : Mat[w,h,T]) : Mat[w,h,T] =
    for i in 0 .. < w:
      for j in 0 .. < h:
        result.arr[i].arr[j] = interal_op(m1.arr[i].arr[j], m2.arr[i].arr[j])

operatorMat( `**`, `*` )
operatorMat( `//`, `/` )
operatorMat( `++`, `+` )
operatorMat( `--`, `-` )
operatorMat( `+`, `+` )
operatorMat( `-`, `-` )

proc `-`*[w,h : static[int], T](m : Mat[w,h,T]) : Mat[w,h,T] =
  for i in 0 .. < w:
    for j in 0 .. < h:
      result.arr[i].arr[j] = -m.arr[i].arr[j]

proc `*`*[h : static[int], T](v : Vec[h,T]; c : T) : Vec[h,T] =
  for i in 0 .. < h:
    result = v.arr[i] * c

proc `*`*[h : static[int], T](c : T; v : Vec[h,T]) : Vec[h,T] =
  for i in 0 .. < h:
    result = c * v.arr[i]

proc `*`*[w,h : static[int], T](m : Mat[w,h,T]; v : Vec[w,T]) : Vec[h,T] =
  for i in 0 .. < h:
    result += m.arr[i] * v

proc `*`*[w,h : static[int], T](m1,m2 : Mat[w,h,T]) : Mat[w,h,T] =
  for i in 0 .. < w:
    result.arr[i] = m1 * m2.arr[i]

proc diag*[h : static[int], T](m: Mat[h,h,T]) : Vec[h,T] =
  for i in 0 .. < h:
    result.arr[i] = m.arr[i].arr[i]

proc diag*[h : static[int], T](v: Vec[h,T]) : Mat[h,h,T] =
  for i in 0 .. < h:
    result.arr[i].arr[i] = v.arr[i]

type
  Vec2f* = Vec[2, float32]
  Vec3f* = Vec[3, float32]
  Vec4f* = Vec[4, float32]
  Mat2f* = Mat[2, 2, float32]
  Mat3f* = Mat[3, 3, float32]
  Mat4f* = Mat[4, 4, float32]
  Mat2x2f* = Mat[2, 2, float32]
  Mat2x3f* = Mat[2, 3, float32]
  Mat2x4f* = Mat[2, 4, float32]
  Mat3x2f* = Mat[3, 2, float32]
  Mat3x3f* = Mat[3, 3, float32]
  Mat3x4f* = Mat[3, 4, float32]
  Mat4x2f* = Mat[4, 2, float32]
  Mat4x3f* = Mat[4, 3, float32]
  Mat4x4f* = Mat[4, 4, float32]
  
  
  Vec2d* = Vec[2, float64]
  Vec3d* = Vec[3, float64]
  Vec4d* = Vec[4, float64]
  Mat2d* = Mat[2, 2, float64]
  Mat3d* = Mat[3, 3, float64]
  Mat4d* = Mat[4, 4, float64]
  Mat2x2d* = Mat[2, 2, float64]
  Mat2x3d* = Mat[2, 3, float64]
  Mat2x4d* = Mat[2, 4, float64]
  Mat3x2d* = Mat[3, 2, float64]
  Mat3x3d* = Mat[3, 3, float64]
  Mat3x4d* = Mat[3, 4, float64]
  Mat4x2d* = Mat[4, 2, float64]
  Mat4x3d* = Mat[4, 3, float64]
  Mat4x4d* = Mat[4, 4, float64]
  
proc vec2f*(x,y : float32 ) : Vec2f = Vec2f(arr : [x,y])

proc vec3f*(x,y,z : float32)    : Vec3f = Vec3f(arr : [x,y,z])
proc vec3f*(v: Vec2f, z: float32) : Vec3f = Vec3f(arr : [v.arr[0], v.arr[1], z])
proc vec3f*(x: float32, v: Vec2f) : Vec3f = Vec3f(arr : [x, v.arr[0], v.arr[1]])

proc vec4f*(x,y,z,w : float32) : Vec4f = Vec4f(arr : [x,y,z,w])
proc vec4f*(v: Vec2f; z,w : float32) : Vec4f = Vec4f(arr : [v.arr[0], v.arr[1], z, w])
proc vec4f*(x:float32; v:Vec2f; w:float32) : Vec4f = Vec4f(arr : [x,v.arr[0], v.arr[1], w])
proc vec4f*(x,y:float32; v:Vec2f) : Vec4f = Vec4f(arr : [x,y, v.arr[0], v.arr[1]])
proc vec4f*(v:Vec3f; w:float32) : Vec4f = Vec4f(arr : [v.arr[0], v.arr[1], v.arr[2], w])
proc vec4f*(x:float32; v:Vec3f) : Vec4f = Vec4f(arr : [x, v.arr[0], v.arr[1], v.arr[2]])

proc mat2f*(x,y : Vec2f) : Mat2f = Mat2f(arr : [x,y])
proc mat3f*(x,y,z : Vec3f) : Mat3f = Mat3f(arr : [x,y,z])
proc mat4f*(x,y,z,w : Vec4f) : Mat4f = Mat4f(arr : [x,y,z,w])

var
  v1 = vec3f(1,2,3)
  v2 = vec3f(4,5,6)
  v3 = vec3f(7,8,9)

  m1 = mat3f(v1,v2,v3)
  m2 = mat3f(v2,v3,v1)
  m3 = mat3f(v3,v1,v2)

echo vec2f(1,2)
echo mat2f(vec2f(1,2), vec2f(3,4))

echo v1 + v2
echo v1 - v2
echo v1 * v2
echo v1 / v2

echo m1 ++ m2
echo m1 -- m2
echo m1 ** m2
echo m1 // m2

echo m1 * m2


