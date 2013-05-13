from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('str_cat', 'has_field', 'object', 'void_arrow')

+ str_cat('get-','a','get-a')

# void_arrow(s, t) if s = void -> t
+ void_arrow('s', 't')

# well formed
+ object('o')
+ has_field('o', 'a', 't')
+ has_field('o', 'get-a', 's')

# well formed
+ object('r')

# ill formed - missing getter
+ object('p')
+ has_field('p', 'a', 't')

# ill formed - getter without field to get
+ object('q')
+ has_field('q', 'get-a', 's')

# ill formed - getter has wrong type
+ object('u')
+ has_field('u', 'a', 't')
+ has_field('u', 'get-a', 't')

##################################################
# Rules

pyDatalog.create_atoms('has_getter_for', 'has_getter', 'ill_formed', 'well_formed', \
  'O','A','B', 'T', 'S')

# Does O have a getter for A?
has_getter_for(O, A) <= \
    has_field(O, A, T) & \
    has_field(O, B, S) & \
    str_cat('get-', A, B) & \
    void_arrow(S, T)

# Does O have a field that B is a getter for?
has_getter(O, B) <= \
    has_field(O, B, S) & \
    has_field(O, A, T) & \
    str_cat('get-', A, B) & \
    void_arrow(S, T)

# Does O have a field that is neither a getter nor gotten?
ill_formed(O) <= \
    has_field(O, A, T) & \
    ~ has_getter(O, A) & \
    ~ has_getter_for(O, A)

# We need to use O in some positive clause before a negated one, so that's
# why we have this uninformative object relation.
well_formed(O) <= \
  object(O) & \
  ~ ill_formed(O)

##################################################
# Queries

print "str_cat"
print(str_cat(A,B,T))
print
print "has_field"
print(has_field(O,A,T))
print
print "has_getter_for"
print(has_getter_for(O,A))
print
print "has_getter"
print(has_getter(O,A))
print
print "ill_formed"
print(ill_formed(O))
print
print "well_formed"
print(well_formed(O))
