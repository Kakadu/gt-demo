module Original = struct
  type 'a x = X of 'a
  and y = int x
  and z = float x
end

type ('a,'b,'c) x_ = X of 'a
type 'a y_ = 'a
type 'a z_ = 'a


type 'a x = ('a,y,z) x_
and y = (int x) y_
and z = (float x) z_
