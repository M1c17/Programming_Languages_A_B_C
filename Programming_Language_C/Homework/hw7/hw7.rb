# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    NoPoints.new # could also have np NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
  def eval_prog env
    self
  end
  def preprocess_prog
    self
  end
end

#A NoPoints represents the empty set of two-dimensional points.
class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end

#A Point represents a two-dimensional point with an x-coordinate and a y-coordinate.
#Both coordinates are floating-point numbers.
class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  def shift(dx,dy)
    Point.new(x + dx, y + dy) 
  end
  # first-distpatch
  def intersect other
    other.intersectPoint self 
  end
  #second-distpatch
  def intersectPoint p
    if real_close_point(x, y, p.x, p.y)
    self
    else NoPoints.new
  end
  def intersectLine line
    if real_close(y, line.m * x + line.b)
    self 
    else NoPoints.new
  end
  def intersectVerticalLine vline
     if real_close(x, vline.x)
     self
     else NoPoints.new
  end
  def intersectWithSegmentAsLineResult seg
    if inbetween(x, seg.x1, seg.x2) and inbetween(y, seg.y1, seg.y2)
      self
    else NoPoints.new
  end
  private
  def inbetween(v, end1, end2)
    epsilon = GeometryExpression::Epsilon
    (end1 - epsilon <= v && v <= end2 + epsilon) or
      (end2 - epsilon <= v && v <= end1 + epsilon)
  end
end

#A Line is a non-vertical infinite line in the plane,
# represented by a slope and an intercept (as in
#  y = mx + b where m is the slope and b is the intercept),
# both floating-point numbers.
class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end
  def shift(dx,dy)
    Line.new(m, b + dy - m * dy) 
  end
  def intersect other
    other.intersectLine self 
  end
  def intersectPoint p
    p.intersectLine self
  end
  def intersectLine line
    if real_close(m, line.m)
      if real_close(b, line.b)
        self
      else NoPoints.new
      end
    else 
      x = (line.b - b) / (m - line.m)
      y = m * x + b
    Point.new(x, y) 
  end
  def intersectVerticalLine vline
    Point.new(vline.x, m * vline.x + b)
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

#A VerticalLine is an infinite vertical line in the plane,
# represented by its x-coordinate.
class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def shift(dx,dy)
    VerticalLine.new(x + dx) 
  end
  def intersect other
    other.intersectVerticalLine self 
  end
  def intersectPoint p
    p.intersectVerticalLine self
  end
  def intersectLine line
    line.intersectVerticalLine self
  end
  def intersectVerticalLine vline
    if real_close(x, vline.x)
      self
    else NoPoints.new
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

#A LineSegment is a (finite) line segment, represented by 
#the x- and y-coordinates of its endpoints 
#(so four total floating-point numbers).
class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end
  def shift(dx,dy)
    LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 +dy)
  end
  def intersect other
    other.intersectLineSegment self 
  end
  def intersectPoint p
    p.intersectLineSegment self
  end
  def intersectLine line
    line.intersectLineSegment self 
  end
  def intersectVerticalLine vline
    vline.intersectVerticalLine self
  end
  def intersectWithSegmentAsLineResult seg
    # the segments are on a vertical line
    if real_close(x1, x2)
      if y1 > seg.y1
        seg.intersectWithSegmentAsLineResult self
      elsif real_close(y2, seg.y1) 
        Point.new(x1,y2) #just touching
      elsif y2 < seg.y1 
        NoPoints.new #disjoint
      elsif y2 > seg.y2
        seg # seg inside self
      else LineSegment.new(x1, seg.y1, x2, y2) # overlapping
      end
    else
      # the segments are on a (non-vertical) line 
      # let segment a start at or to the left of start of segment b
      if x1 > seg.x1
        seg.intersectWithSegmentAsLineResult self
      elsif real_close(x2, seg.x1)
        Point.new(x2,y2) #just touching
      elsif x2 < seg.x1
        NoPoints.new #disjoint
      elsif x2 > seg.x2
        seg # seg inside self
      else LineSegment.new(seg.x1, seg.y1, x2, y2) # overlapping
      end
    end
  end
  def preprocess_prog
    if real_close_point(x1, y1, x2, y2)
      Point.new(x1, y1)
    elsif real_close(x1, x2)
      LineSegment.new(x2, y2, x1, y1)
    elsif x1 > x2
      LineSegment.new(x2, y2, x1, y1)
    else
      self
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

#An Intersect expression is not a value. It has two subexpressions. The semantics is to evaluate the
#subexpressions (in the same environment) and then return the value that is the intersection (in the
#geometric sense) of the two subresults. For example, the intersection of two lines could be one of:
#– NoPoints, if the lines are parallel
#– a Point, if the lines intersect
#– a Line, if the lines have the same slope and intercept (see the note below about what “the same”
#means for floating-point numbers)
class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  
  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end

#A Let expression is not a value. It is like let-expressions in other languages we have studied: The first
#subexpression is evaluated and the result bound to a variable that is added to the environment for
#evaluating the second subexpression.
class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  def eval_prog env
    @e2.eval_prog([[@s, @e1.eval_prog(env)]] + env)
  end
  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end
end

#A Var expression is not a value. It is for using variables in the environment: We look up a string in
#the environment to get a geometric value.
class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
  def preprocess_prog
    self
  end
end

#A Shift expression is not a value. It has a deltaX (a floating-point number), a deltaY (a floating-point
#  number), and a subexpression. The semantics is to evaluate the subexpression and then shift the result
#  by deltaX (in the x-direction; positive is “to the right”) and deltaY (in the y-direction; positive is
#  “up”). More specifically, shifting for each form of value is as follows:
#  – NoPoints remains NoPoints.
#  – A Point representing (x, y) becomes a Point representing (x + deltaX , y + deltaY ).
#  – A Line with slope m and intercept b becomes a Line with slope m and an intercept of
#  b + deltaY − m · deltaX .
#  – A VerticalLine becomes a VerticalLine shifted by deltaX ; the deltaY is irrelevant.
#  – A LineSegment has its endpoints shift by deltaX and deltaY .
class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end
  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end