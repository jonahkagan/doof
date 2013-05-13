from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'bean_pair', 'str_cat', \
    'get_type', 'set_type')

+ has_field('o1', 'a', 't')
+ has_field('o1', 'b', 's')
+ bean_pair('o1', 'bean1')

+ bean_pair('o2', 'bean2')

+ str_cat('get-', 'a', 'get-a')
+ str_cat('get-', 'b', 'get-b')
+ str_cat('set-', 'a', 'set-a')
+ str_cat('set-', 'b', 'set-b')

+ get_type('t', '()->t')
+ get_type('s', '()->s')

+ set_type('t', 't->()')
+ set_type('s', 's->()')

##################################################
# Rules

pyDatalog.create_atoms('O', 'Bean', 'A', 'B', 'T', 'S')

# Bean should have 'field' if O has 'field'
has_field(Bean, A, T) <= \
    bean_pair(O, Bean) & \
    has_field(O, A, T)

# Bean should have 'get-field' if O has 'field'
has_field(Bean, B, S) <= \
    bean_pair(O, Bean) & \
    has_field(O, A, T) & \
    str_cat('get-', A, B) & \
    get_type(T, S)

# Bean should have 'set-field' if O has 'field'
has_field(Bean, B, S) <= \
    bean_pair(O, Bean) & \
    has_field(O, A, T) & \
    str_cat('set-', A, B) & \
    set_type(T, S)

##################################################
# Queries

print "has_field(bean1, A, T)"
print(has_field('bean1',A,T))
print
print "has_field(bean2, A, T)"
print(has_field('bean2',A,T))
print
