from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'caja_pair', 'str_cat')

+ has_field('o1', 'a', 't')
+ has_field('o1', 'b', 's')

# caja_pair(o, c) if c = freeze(o)
+ caja_pair('o1', 'c1')

+ str_cat('a', '_v__', 'a_v__')
+ str_cat('b', '_v__', 'b_v__')
+ str_cat('a', '_w__', 'a_w__')
+ str_cat('b', '_w__', 'b_w__')

##################################################
# Rules

pyDatalog.create_atoms('O', 'C', 'A', 'B', 'T')

# 'field' is copied over
has_field(C, A, T) <= \
    caja_pair(O, C) & \
    has_field(O, A, T)

# 'field_v__' is added with same type
has_field(C, B, T) <= \
    caja_pair(O, C) & \
    has_field(O, A, T) & \
    str_cat(A, '_v__', B)

# 'field_w__' is added with bool
has_field(C, B, 'bool') <= \
    caja_pair(O, C) & \
    has_field(O, A, T) & \
    str_cat(A, '_w__', B)

##################################################
# Queries

print 'has_field'
print(has_field(O, A, T))
print
print 'c1'
print(has_field('c1', A, T))
print
