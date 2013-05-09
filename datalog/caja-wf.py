from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'caja_pair', 'str_cat')

+ has_field('o1', 'a', 't')
+ has_field('o1', 'b', 's')

# well formed
+ has_field('c1', 'a', 't')
+ has_field('c1', 'a_v__', 't')
+ has_field('c1', 'a_w__', 'bool')
+ has_field('c1', 'b', 's')
+ has_field('c1', 'b_v__', 's')
+ has_field('c1', 'b_w__', 'bool')

# ill formed - missing b
+ has_field('c2', 'a', 't')
+ has_field('c2', 'a_v__', 't')
+ has_field('c2', 'a_w__', 'bool')
+ has_field('c2', 'b_v__', 's')
+ has_field('c2', 'b_w__', 'bool')

# ill formed - a_v__ has wrong type
+ has_field('c3', 'a', 't')
+ has_field('c3', 'a_v__', 's')
+ has_field('c3', 'a_w__', 'bool')
+ has_field('c3', 'b', 's')
+ has_field('c3', 'b_v__', 's')
+ has_field('c3', 'b_w__', 'bool')

# ill formed - missing b_w__
+ has_field('c4', 'a', 't')
+ has_field('c4', 'a_v__', 't')
+ has_field('c4', 'a_w__', 'bool')
+ has_field('c4', 'b', 's')
+ has_field('c4', 'b_v__', 's')

# caja_pair(o, c) if c = freeze(o)
+ caja_pair('o1', 'c1')
+ caja_pair('o1', 'c2')
+ caja_pair('o1', 'c3')
+ caja_pair('o1', 'c4')

+ str_cat('a', '_v__', 'a_v__')
+ str_cat('b', '_v__', 'b_v__')
+ str_cat('a', '_w__', 'a_w__')
+ str_cat('b', '_w__', 'b_w__')

##################################################
# Rules

pyDatalog.create_atoms('ill_formed', 'well_formed', 'O', 'C', 'A', 'B', 'T')

# 'field' must be copied over
ill_formed(O, C) <= \
    caja_pair(O, C) & \
    has_field(O, A, T) & \
    ~ has_field(C, A, T)

# 'field_v__' must be added with same type
ill_formed(O, C) <= \
    caja_pair(O, C) & \
    has_field(O, A, T) & \
    str_cat(A, '_v__', B) & \
    ~ has_field(C, B, T) 

# 'field_v__' must be added with bool
ill_formed(O, C) <= \
    caja_pair(O, C) & \
    has_field(O, A, T) & \
    str_cat(A, '_w__', B) & \
    ~ has_field(C, B, 'bool')

# Do we need a way to say that C shouldn't have any other fields besides the
# ones added by the Caja transformation of O?

well_formed(O, C) <= \
    caja_pair(O, C) & \
    ~ ill_formed(O, C)

##################################################
# Queries

print 'has_field'
print(has_field(O, A, T))
print
print 'ill_formed'
print(ill_formed(O, C))
print
print 'well_formed'
print(well_formed(O, C))
print
