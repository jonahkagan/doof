from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'caja_output', 'str_cat')

# well formed
+ caja_output('c1')
+ has_field('c1', 'a', 't')
+ has_field('c1', 'a_v__', 't')
+ has_field('c1', 'a_w__', 'bool')
+ has_field('c1', 'b', 's')
+ has_field('c1', 'b_v__', 's')
+ has_field('c1', 'b_w__', 'bool')

# ill formed - missing b
+ caja_output('c2')
+ has_field('c2', 'a', 't')
+ has_field('c2', 'a_v__', 't')
+ has_field('c2', 'a_w__', 'bool')
+ has_field('c2', 'b_v__', 's')
+ has_field('c2', 'b_w__', 'bool')

# ill formed - a_v__ has wrong type
+ caja_output('c3')
+ has_field('c3', 'a', 't')
+ has_field('c3', 'a_v__', 's')
+ has_field('c3', 'a_w__', 'bool')
+ has_field('c3', 'b', 's')
+ has_field('c3', 'b_v__', 's')
+ has_field('c3', 'b_w__', 'bool')

# ill formed - missing b_w__
+ caja_output('c4')
+ has_field('c4', 'a', 't')
+ has_field('c4', 'a_v__', 't')
+ has_field('c4', 'a_w__', 'bool')
+ has_field('c4', 'b', 's')
+ has_field('c4', 'b_v__', 's')

+ str_cat('a', '_v__', 'a_v__')
+ str_cat('b', '_v__', 'b_v__')
+ str_cat('a', '_w__', 'a_w__')
+ str_cat('b', '_w__', 'b_w__')

##################################################
# Rules

pyDatalog.create_atoms('ill_formed', 'well_formed', \
  'has_v', 'has_v_for', 'has_w', 'has_w_for', \
  'C', 'A', 'B', 'T', 'S')

# Does C have a field A_v__?
has_v_for(C, A) <= \
    has_field(C, A, T) & \
    has_field(C, B, S) & \
    str_cat(A, '_v__', B) & \
    (T == S)

# Does C have a field B that is A_v__?
has_v(C, B) <= \
    has_field(C, A, T) & \
    has_field(C, B, S) & \
    str_cat(A, '_v__', B) & \
    (T == S)

# Does C have a field A_w__?
has_w_for(C, A) <= \
    has_field(C, A, T) & \
    has_field(C, B, 'bool') & \
    str_cat(A, '_w__', B)

# Does C have a field B that is A_w__?
has_w(C, B) <= \
    has_field(C, A, T) & \
    has_field(C, B, 'bool') & \
    str_cat(A, '_w__', B)

ill_formed(C) <= \
    has_field(C, A, T) & \
    ~ (has_v_for(C, A) & has_w_for(C, A)) & \
    ~ has_v(C, A) & \
    ~ has_w(C, A)

well_formed(C) <= \
    caja_output(C) & \
    ~ ill_formed(C)

##################################################
# Queries

print 'ill_formed'
print(ill_formed(C))
print
print 'well_formed'
print(well_formed(C))
print
